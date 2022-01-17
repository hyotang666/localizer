(in-package :cl-user)

(defpackage :localizer.core
  (:use :cl)
  (:export ;;;; DSL
           #:defdict
           ;;;; CONFIGURATION
           #:*key-predicate*
           ;;;; DICTIONARY
           #:add-translation
           #:find-translated
           #:do-dict
           ;;;; LANGUAGE
           #:language ; Type
           #:language-dictionary ; Reader
           #:find-language
           ;;;; TYPES
           #:text-designator
           #:lang-name))

(in-package :localizer.core)

(declaim (optimize speed))

;;;; Object DICTIONARY

(deftype dictionary () 'hash-table)

;;; CONFIGURATIONS

(defparameter *key-predicate*
  #'equalp
  "To use dictionary key test. The default is equalp as case-insensitive.")

;;; CREATE

(defun make-dictionary () (make-hash-table :test *key-predicate*))

;;; REFER

(defun find-translated (origin dictionary) (values (gethash origin dictionary)))

;;; UPDATE

(defun add-translation (origin translated dictionary)
  (setf (gethash origin dictionary) translated))

;;; DELETE

(defun delete-translation (origin dictionary) (remhash origin dictionary))

;;; ITERATOR

(defmacro do-dict ((var <dictionary> &optional <return>) &body body)
  (setf var (uiop:ensure-list var))
  (assert (null (cddr var)))
  `(loop :for ,(car var) :being :each :hash-key :of
              ,<dictionary> ,@(when (cdr var)
                                `(:using (:hash-value ,(cadr var))))
         :do (tagbody ,@body)
         :finally (return ,<return>)))

;;; PRINT
;; none.
;;;; Object LANGUAGE

(defvar *languages* (make-hash-table :test #'eq) "Repository of languages.")

(deftype lang-name () 'keyword)

(defstruct (language (:constructor %make-language) (:conc-name lang-))
  (name (error "NAME is required.") :type lang-name :read-only t)
  (aliases nil :type list #|of lang-name|#)
  (dictionary (make-dictionary) :type dictionary :read-only t))

(deftype language-designator () '(or lang-name language))

;;; PRINT

(defmethod print-object ((l language) output)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (l output :type t :identity nil)
        (funcall (formatter "~W~^ ~{~W~^ ~}") output (language-name l)
                 (language-aliases l)))))

;;; CREATE

(defun make-language (&key name aliases translations)
  (loop :with dict = (make-dictionary)
        :for (origin translated) :on translations :by #'cddr
        :do (add-translation origin translated dict)
        :finally (return
                  (%make-language :name name
                                  :aliases aliases
                                  :dictionary dict))))

(defun store-language (language)
  (setf (gethash (language-name language) *languages*) language))

;;; REFER

(declaim
 (ftype (function (lang-name language) (values t &optional)) match-lang-name-p)
 (ftype (function (language-designator) (values lang-name &optional))
        language-name)
 (ftype (function (language-designator) (values list &optional))
        language-aliases)
 (ftype (function (language-designator) (values dictionary &optional))
        language-dictionary))

(defun match-lang-name-p (lang-name language)
  (or (eq lang-name (language-name language))
      (find lang-name (language-aliases language))))

(defun find-language (lang-name &optional (errorp t))
  (loop :for language :being :each :hash-value :of *languages*
        :if (match-lang-name-p lang-name language)
          :return language
        :finally (and errorp (error "Missing language named ~S." lang-name))))

(defun list-all-languages ()
  (loop :for language :being :each :hash-value :of *languages*
        :collect language))

(defun ensure-language (language)
  (etypecase language (language language) (keyword (find-language language))))

(defun language-name (designator) (lang-name (ensure-language designator)))

(defun language-aliases (designator)
  (lang-aliases (ensure-language designator)))

(defun language-dictionary (designator)
  (lang-dictionary (ensure-language designator)))

;;; UPDATE
;; none.
;;; DELETE

(defun delete-language (lang-name)
  (loop :for language :being :each :hash-value :of *languages*
        :if (match-lang-name-p lang-name language)
          :do (remhash lang-name *languages*)
              (loop-finish)))

;;; ITERATE
;; none.
;;;; DSL DEFDICT

(deftype text-designator () '(or symbol string))

(defun parse-name-spec (name-spec)
  (if (atom name-spec)
      (values name-spec nil)
      (values (car name-spec) (cdr name-spec))))

(defmacro defdict (name-spec &rest translations)
  "(defdict [ lang-name | (lang-name { (:aliases lang-name+) }) ] &rest { origin translated }*)
  lang-name := KEYWORD
  origin := (OR SYMBOL STRING)
  translated := (OR SYMBOL STRING)"
  (multiple-value-bind (name options)
      (parse-name-spec name-spec)
    (let ((aliases (cdr (assoc :aliases options))))
      ;; Trivial-syntax-check.
      (assert (typep name 'lang-name))
      (assert (null (remove :aliases options :key #'car)) ()
        "Unknown option comes. ~S" options)
      (assert (every (lambda (name) (typep name 'lang-name)) aliases))
      (assert (every (lambda (text) (typep text 'text-designator))
                     translations))
      ;; The body.
      `(progn
        (store-language
          (make-language :name ,name
                         :aliases ',aliases
                         :translations ',translations))
        ,name))))

(defun pprint-defdict (stream exp)
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-logical-block.
                    "~W~^ ~1I~:_" ; operator.
                    "~:<" ; pprint-logical-block for name-spec.
                    "~W~^ ~:I~:_" ; name.
                    "~@{~W~^ ~:_~}" ; aliases.
                    "~:>~^ ~_" "~@{" ; translations
                    "~W~^ ~:_" ; origin.
                    "~W~^ ~_" ; translated.
                    "~}" ; end of translasions.
                    "~:>"))
    stream exp))

(set-pprint-dispatch '(cons (member defdict)) 'pprint-defdict)