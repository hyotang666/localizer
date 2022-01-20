(in-package :cl-user)

#.(flet ((reexport (symbols)
           `((:import-from :localizer.core ,@symbols) (:export ,@symbols))))
    `(defpackage :localizer
       (:use :cl)
       ,@(reexport '(#:defdict #:*key-predicate*))
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
                #:*break-on-missing*
                ;;;; Helpers
                #:store-as-default
                #:lack-middleware)))

(in-package :localizer)

(declaim (optimize speed))

;;; CONFIGURATIONS

(defdict :en)

(declaim (type localizer.core:language *language*))

(defvar *language* (localizer.core:find-language :en) "Current language.")

(defparameter *default-language* :en)

(declaim
 (ftype (function (localizer.core:text-designator)
         (values localizer.core:text-designator &optional))
        store-as-default))

(defun store-as-default (target)
  "Store TARGET as default-language. Intended to be bound by *break-on-missing*."
  #+(or ccl clisp abcl allegro cmu)
  (assert (typep target 'localizer.core:text-designator))
  (localizer.core:add-translation target target
                                  (localizer.core:language-dictionary
                                    *default-language*))
  target)

(defparameter *break-on-missing*
  'store-as-default
  "Function designator. This is called when word is missing in current dictionary.")

;;; MAIN FEATURES

(defun written-p (key)
  (localizer.core:find-translated key
                                  (localizer.core:language-dictionary
                                    *language*)))

(define-compiler-macro localize (&whole whole target &environment env)
  (when (constantp target env)
    (let ((target (eval target)))
      (check-type target localizer.core:text-designator)
      (store-as-default target)))
  whole)

(declaim
 (ftype (function (localizer.core:text-designator) (values t &optional))
        localize))

(defun localize (target)
  "Localize TARGET if current dictionary have its definition.
  Otherwise call *BREAK-ON-MISSING*."
  #+(or ccl clisp abcl allegro cmu)
  (assert (typep target 'localizer.core:text-designator))
  (or (written-p target)
      (funcall (coerce *break-on-missing* 'function) target)))

;;; TEMPLATE

(declaim
 (ftype (function (localizer.core:lang-name &rest localizer.core:lang-name)
         (values cons &optional))
        template))

(defun template (lang-name &rest aliases)
  "Print template for DEFDICT."
  #+(or allegro clisp abcl)
  (check-type lang-name keyword)
  (assert (every (lambda (name) (typep name 'localizer.core:lang-name))
                 aliases))
  (let ((*package* (find-package :cl-user)))
    (print
      `(defdict
         ,(if aliases
              `(,lang-name (:aliases ,aliases))
              lang-name)
         ,@(uiop:while-collecting (acc)
             (localizer.core:do-dict ((key value)
                                      (localizer.core:language-dictionary
                                        *default-language*))
               (acc key)
               (acc
                (let ((*break-on-missing* (constantly value)))
                  (localize key)))))))))

;;; LACK-MIDDLEWARE

(declaim
 (ftype (function (string) (values list &optional)) parse-accept-language))

(defun parse-accept-language (accept-language)
  "Return list of LANGUAGEs that is sorted by great to less quality order."
  (let ((table (make-hash-table)))
    (labels ((hash-keys (table)
               (loop :for key :being :each :hash-key :of table
                     :collect key :into keys
                     :finally (return (sort keys #'>))))
             (parse-quality (notation)
               (subseq notation
                       (1+
                         (or (position #\= notation)
                             (error "Missing = for quality. ~S"
                                    accept-language)))))
             (parse (temp section)
               (destructuring-bind
                   (language &optional quality)
                   (uiop:split-string section :separator ";" :max 2)
                 (declare (type (or null simple-string) quality))
                 (setf language
                         (uiop:safe-read-from-string language
                                                     :package :keyword))
                 (if quality
                     (tagbody ; implicitly return nil.
                       (setf (gethash
                               (uiop:safe-read-from-string
                                 (parse-quality quality)
                                 :package :keyword)
                               table)
                               (nreconc temp (list language))))
                     (cons language temp)))))
      (reduce #'parse (uiop:split-string accept-language :separator ",")
              :initial-value nil)
      (loop :for key :in (hash-keys table)
            :nconc (gethash key table)))))

(declaim
 (ftype (function (string) (values localizer.core:language &optional))
        detect-accept-language))

(defun detect-accept-language (accept-language)
  (loop :for lang-name :in (parse-accept-language accept-language)
        :when (localizer.core:find-language lang-name nil)
          :return :it
        :finally (return (localizer.core:find-language *default-language*))))

(declaim
 (ftype (function (function) (values function &optional)) lack-middleware))

(defun lack-middleware (app)
  (lambda (env)
    (let ((*language*
           (detect-accept-language
             (gethash "accept-language" (getf env :headers)))))
      (funcall app env))))

;;; READER

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
