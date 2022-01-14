(in-package :cl-user)

(defpackage :localizer
  (:use :cl)
  (:export ;;;; Reader macro.
           #:set-syntax
           #:|#L-reader|
           ;;;; Template.
           #:template
           ;;;; Configurations.
           #:*default-language*
           #:*language*
           #:*beak-on-missing*
           #:store-as-default
           #:parse-accept-language))

(in-package :localizer)

(declaim (optimize speed))

(deftype language () 'keyword)

(deftype dictionary () 'hash-table)

(defvar *dictionaries*
  (make-hash-table :test #'eq)
  "Repository of dictionaries as {keyword:hash-table}")

(defun make-dictionary () (make-hash-table :test #'equal))

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

(defun written-p (key &optional (dictionary (find-dictionary *language*)))
  (values (gethash key dictionary)))

(defmacro defdict (language &body definition*)
  ;; trivial syntax check.
  (check-type language keyword)
  (loop :for (key def) :on definition* :by #'cddr
        :do (check-type key (or symbol string))
            (check-type def string))
  (let ((?dictionary (gensym "DICTIONARY")))
    `(let ((,?dictionary (make-dictionary)))
       (add-words ,?dictionary ,@definition*)
       (store-dictionary ,language ,?dictionary)
       ,language)))

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

(defun localize (target)
  (or (written-p target)
      (funcall (coerce *break-on-missing* 'function) target)))

(defun |#L-reader| (input sub-char num-arg)
  (declare (ignore sub-char num-arg))
  `(localize ,(read input t t t)))

(declaim
 (ftype (function (&optional character) (values t &optional)) set-syntax))

(defun set-syntax (&optional (sub-char #\l))
  "Set dispatch macro character with SUB-CHAR for |#L-reader|. Continuable."
  (let ((reader (get-dispatch-macro-character #\# sub-char)))
    (if reader
        (if (eq reader '|#L-reader|) ; It's me!
            nil ; do nothing.
            (progn
             (cerror "Force to set." "#~C dispatch macro is already used. ~S"
                     reader)
             #0=(set-dispatch-macro-character #\# sub-char '|#L-reader|)))
        #0#)))

(declaim (ftype (function (language) (values cons &optional)) template))

(defun template (language)
  "Print template for DEFDICT."
  (print
    `(defdict ,language
       ,@(uiop:while-collecting (acc)
           (do-dict ((key value) (find-dictionary *default-language*))
             (acc key)
             (acc value))))))

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
                 :finally (return (sort keys #'>)))))
    (loop :with table = (make-hash-table)
          :for section :in (uiop:split-string accept-language :separator ",")
          :for (language . quality)
               = (uiop:split-string section :separator ";" :max 2)
          :if quality
            :do (setf (gethash
                        (safe-read-from-string
                          (let ((quality (car quality)))
                            (declare (simple-string quality))
                            (subseq quality
                                    (1+
                                      (or (position #\= quality)
                                          (error "Missing #= for quality. ~S"
                                                 accept-language))))))
                        table)
                        group)
          :collect (safe-read-from-string language) :into group
          :finally (return
                    (loop :for key :in (hash-keys table)
                          :nconc (gethash key table))))))