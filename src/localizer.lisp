(in-package :cl-user)

(defpackage :localizer
  (:use :cl)
  (:export ;;;; Reader macro.
           #:set-syntax
           #:|#L-reader|
           #:localize
           ;;;; DSL
           #:defdict
           ;;;; Template.
           #:template
           ;;;; Configurations.
           #:*default-language*
           #:*language*
           #:*beak-on-missing*
           #:*key-predicate*
           ;;;; Helpers
           #:store-as-default
           #:detect-accept-language
           #:delete-dictionary))

(in-package :localizer)

(declaim (optimize speed))

(deftype language () 'keyword)

(deftype dictionary () 'hash-table)

(defvar *dictionaries*
  (make-hash-table :test #'eq)
  "Repository of dictionaries as {keyword:hash-table}")

(defparameter *key-predicate*
  #'equalp
  "To use dictionary key test. The default is equalp as case-insensitive.")

(defun make-dictionary () (make-hash-table :test *key-predicate*))

(declaim
 (ftype (function (language dictionary) (values dictionary &optional))
        store-dictionary))

(defun store-dictionary (language dictionary)
  (setf (gethash language *dictionaries*) dictionary))

(defun delete-dictionary (language) (remhash language *dictionaries*))

(defun find-dictionary (language &optional (errorp t))
  (or (gethash language *dictionaries*)
      (and errorp (error "Missing dictionary for ~S." language))))

(defmacro do-dict
          (((key &optional definition) <language> &optional <return>)
           &body body)
  `(loop :for ,key :being :each :hash-key :of
              (find-dictionary ,<language>) ,@(and definition
                                                   `(:using
                                                     (:hash-value ,definition)))
         :do (tagbody ,@body)
         :finally (return ,<return>)))

(defun add-words (dictionary &rest definition*)
  (loop :for (key def) :on definition* :by #'cddr
        :do (setf (gethash key dictionary) def)))

(defvar *language* :en "Current language.")

(defun written-p (key &optional (dictionary (find-dictionary *language* nil)))
  (when dictionary
    (values (gethash key dictionary))))

(defmacro defdict (language &body definition*)
  (setq language (uiop:ensure-list language))
  ;; trivial syntax check.
  (assert (every #'keywordp language))
  (loop :for (key def) :on definition* :by #'cddr
        :do (check-type key (or symbol string))
            (check-type def (or symbol string)))
  (let ((?dictionary (gensym "DICTIONARY")) (?lang (gensym "LANG")))
    `(let ((,?dictionary (make-dictionary)))
       (add-words ,?dictionary ,@definition*)
       (dolist (,?lang ',language) (store-dictionary ,?lang ,?dictionary))
       ',language)))

(store-dictionary :en (make-dictionary))

(defun pprint-defdict (stream exp)
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-logical-block.
                    "~W~^ ~:_" ; operator.
                    "~W~^~1I ~_" ; language.
                    "~@{" ; definitions.
                    "~W~^ ~:_" ; key
                    "~W~^ ~_" ; def
                    "~}" ; end of definitions.
                    "~:>"))
    stream exp))

(set-pprint-dispatch '(cons (member defdict)) 'pprint-defdict)

(defparameter *default-language* :en)

(defun store-as-default (target)
  "Store TARGET as default-language. Intended to be bound by *break-on-missing*."
  (add-words (find-dictionary *default-language*) target target)
  target)

(defparameter *break-on-missing*
  'store-as-default
  "Function designator. This is called when word is missing in current dictionary.")

(define-compiler-macro localize (&whole whole target &environment env)
  (when (constantp target env)
    (store-as-default target))
  whole)

(declaim (ftype (function (t) (values string &optional)) localize))

(defun localize (target)
  (or (written-p target)
      (funcall (coerce *break-on-missing* 'function) target)))

(defun |#L-reader| (input sub-char num-arg)
  (declare (ignore sub-char num-arg))
  `(localize ,(read input t t t)))

(defmacro set-syntax (&optional (sub-char #\l))
  "Set dispatch macro character with SUB-CHAR for |#L-reader|. Continuable."
  (check-type sub-char character)
  (let ((?reader (gensym "READER")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,?reader (get-dispatch-macro-character #\# ',sub-char)))
         (if ,?reader
             (if (eq ,?reader '|#L-reader|) ; It's me!
                 nil ; do nothing.
                 (progn
                  (cerror "Force to set."
                          "#~C dispatch macro is already used. ~S" ,?reader)
                  #0=(set-dispatch-macro-character #\# ',sub-char
                                                   '|#L-reader|)))
             #0#)))))

(declaim (ftype (function (language) (values cons &optional)) template))

(defun template (*language*)
  "Print template for DEFDICT."
  (print
    `(defdict ,*language*
       ,@(uiop:while-collecting (acc)
           (do-dict ((key value) *default-language*)
             (acc key)
             (acc
              (let ((*break-on-missing* (constantly value)))
                (localize key))))))))

(declaim
 (ftype (function (string) (values list &optional)) parse-accept-language))

(defun parse-accept-language (accept-language)
  "Return list of LANGUAGEs that is sorted by great to less quality order."
  (flet ((safe-read-from-string (string)
           (let ((*read-eval* nil) (*package* (find-package :keyword)))
             (read-from-string string)))
         (hash-keys (table)
           (loop :for key :being :each :hash-key :of table
                 :collect key :into keys
                 :finally (return (sort keys #'>))))
         (parse-quality (notation)
           (subseq notation
                   (1+
                     (or (position #\= notation)
                         (error "Missing = for quality. ~S"
                                accept-language))))))
    (let ((table (make-hash-table)) (acc nil))
      (dolist
          (section (uiop:split-string accept-language :separator ",")
                   (loop :for key :in (hash-keys table)
                         :append (gethash key table)))
        (destructuring-bind
            (language &optional quality)
            (uiop:split-string section :separator ";" :max 2)
          (declare (type (or null simple-string) quality))
          (push (safe-read-from-string language) acc)
          (when quality
            (setf (gethash (safe-read-from-string (parse-quality quality))
                           table)
                    (nreverse acc)
                  acc nil)))))))

(declaim
 (ftype (function (string) (values keyword &optional)) detect-accept-language))

(defun detect-accept-language (accept-language)
  (loop :for lang-name :in (parse-accept-language accept-language)
        :when (find-dictionary lang-name nil)
          :return lang-name
        :finally (return *default-language*)))