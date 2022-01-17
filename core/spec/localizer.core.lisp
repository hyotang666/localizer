(defpackage :localizer.core.spec
  (:use :cl :jingoh :localizer.core))
(in-package :localizer.core.spec)
(setup :localizer.core)

(requirements-about DEFDICT :doc-type function)

;;;; Description:

#+syntax (DEFDICT NAME-SPEC &BODY TRANSLATION*) ; => result

;;;; Arguments and Values:

; name-spec := [ lang-name | (lang-name (:aliases lang-name*)) ]
; lang-name := keyword
; otherwise signals error.
#?(defdict "not keyword") :signals error
,:lazy t
#?(defdict ()) :signals error
,:lazy t
#?(defdict ("not keyword")) :signals error
,:lazy t
#?(defdict (:dummy (:unknown))) :signals error

; definition* := { key def }*
; key := [ symbol | string ], otherwise signals error.
#?(defdict :en (not keyword or string) "dummy") :signals error

; def := [ symbol | string ], otherwise signals error.
#?(defdict :en "dummy" (not keyword or string)) :signals error

; result := list of LANGUAGE.

#?(defdict :en) => :EN

;;;; Affected By:
; none

;;;; Side-Effects:
; localizer::*DICTIONARIES*
#?(values (find-language :ja nil)
	  (defdict :ja)
	  (find-language :ja))
:multiple-value-satisfies (lambda (a b c)
			    (& (null a)
			       (equal b :JA)
			       (typep c 'language)))
,:around (let ((localizer.core::*languages* (make-hash-table)))
	   (call-body))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *KEY-PREDICATE* :doc-type variable)

;;;; Description:
; To use dictionary key test. The default is equalp as case-insensitive.

;;;; Value type is COMPILED-FUNCTION
#? *KEY-PREDICATE* :be-the (or symbol function)

; Initial value is `#<FUNCTION EQUALP>`

;;;; Affected By:

;;;; Notes:

