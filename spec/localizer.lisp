(defpackage :localizer.spec
  (:import-from :localizer #:parse-accept-language)
  (:use :cl :jingoh :localizer))
(in-package :localizer.spec)
(setup :localizer)

(requirements-about SET-SYNTAX :doc-type function
		    :around
		    (let ((*readtable* (copy-readtable nil)))
		      (call-body)))

;;;; Description:
; Set dispatch macro character with SUB-CHAR for |#L-reader|. Continuable.

#+syntax (SET-SYNTAX &OPTIONAL (SUB-CHAR #\l)) ; => result

;;;; Arguments and Values:

; sub-char := character, otherwise signals type-error.
#?(set-syntax "not character") :signals type-error
,:lazy t

; result := implementation dependent.
#?(set-syntax) => implementation-dependent

;;;; Affected By:
; *READTABLE*
; If specified dispatch macro is already exists, continuable error is signaled.
#?(set-syntax) :signals error
,:with-restarts (continue)
,:before (set-dispatch-macro-character #\# #\l (get-dispatch-macro-character #\# #\())

;;;; Side-Effects:
; Modify *READTABLE*.
#?(not (eq (get-dispatch-macro-character #\# #\l)
	   (progn (set-syntax)
		  (get-dispatch-macro-character #\# #\l))))
=> T

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFDICT :doc-type function)

;;;; Description:

#+syntax (DEFDICT LANGUAGE &BODY DEFINITION*) ; => result

;;;; Arguments and Values:

; language := [ lang-name | ( lang-name+ ) ]
; lang-name := keyword
; otherwise signals error.
#?(defdict "not keyword") :signals error
,:lazy t
#?(defdict ()) :signals error
,:lazy t
#?(defdict ("not keyword")) :signals error
,:lazy t

; definition* := { key def }*
; key := [ symbol | string ], otherwise signals error.
#?(defdict :en (not keyword or string) "dummy") :signals error

; def := [ symbol | string ], otherwise signals error.
#?(defdict :en "dummy" (not keyword or string)) :signals error

; result := list of LANGUAGE.

#?(defdict :en) => (:en)
,:test equal

;;;; Affected By:
; none

;;;; Side-Effects:
; localizer::*DICTIONARIES*
#?(values (localizer::find-dictionary :ja nil)
	  (defdict :ja)
	  (localizer::find-dictionary :ja))
:multiple-value-satisfies (lambda (a b c)
			    (& (null a)
			       (equal b '(:JA))
			       (hash-table-p c)))
,:around (let ((localizer::*dictionaries* (make-hash-table)))
	   (call-body))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LOCALIZE :doc-type function
		    :test equal
		    :lazy t ; to skip compiler macro.
		    :around
		    (let ((*break-on-missing* #'identity))
		      (call-body)))

;;;; Description:

#+syntax (LOCALIZE TARGET) ; => result

;;;; Arguments and Values:

; target := (or string symbol), otherwise implementation dependent condition.
#?(localize '(not (or symbol string))) :signals condition

; result := t

;;;; Affected By:
; *LANGUAGE*
#?(defdict :test "foo" "bar") => (:TEST)
; If TARGET exists in current language dictionary, such definition is returned.
#?(let ((*language* :test))
    (localize "foo"))
=> "bar"

; If TARGET does not exist in current language dictionary, *BREAK-ON-MISSING* is FUNCALLed.
; *BREAK-ON-MISSING*
#?(let ((*break-on-missing* #'princ)
	(word "unknown"))
    (localize word))
:outputs "unknown"

;;;; Side-Effects:
; If TARGET does not exists in current dictionary, TARGET is stored in default language dictioinary.
#?(localizer::find-dictionary :en)
:satisfies (lambda (dict)
	     (& (hash-table-p dict)
		(eql 1 (hash-table-count dict))
		(equal "foo" (gethash "foo" dict))))

;;;; Notes:
; LOCALIZE has compier-macro.
#?(compiler-macro-function 'localize) :be-the function
; Compiler macro responds to collect TARGET when the argument is literal object.

;;;; Exceptional-Situations:

(requirements-about TEMPLATE :doc-type function)

;;;; Description:
; Print template for DEFDICT.
#?(template :new)
:outputs "
(DEFDICT :NEW \"foo\" \"foo\") "

#+syntax (TEMPLATE *LANGUAGE*) ; => result

;;;; Arguments and Values:

; *language* := keyword, otherwise implementation dependent condition.
#?(template "not keyword") :signals condition

; result := cons
#?(template :new)
=> (DEFDICT :NEW "foo" "foo")
,:stream nil
,:test equal

;;;; Affected By:
; Dictionary status of *DEFAULT-LANGUAGE*.

;;;; Side-Effects:
; Print to *STANDARD-OUTPUT*.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about *DEFAULT-LANGUAGE* :doc-type variable)

;;;; Description:

;;;; Value type is KEYWORD
#? *DEFAULT-LANGUAGE* :be-the keyword

; Initial value is `:EN`

;;;; Affected By:
; none.

;;;; Notes:

(requirements-about *LANGUAGE* :doc-type variable)

;;;; Description:
; Current language.

;;;; Value type is KEYWORD
#? *LANGUAGE* :be-the keyword

; Initial value is `:EN`

;;;; Affected By:
; none.

;;;; Notes:

(requirements-about *KEY-PREDICATE* :doc-type variable)

;;;; Description:
; To use dictionary key test. The default is equalp as case-insensitive.

;;;; Value type is COMPILED-FUNCTION
#? *KEY-PREDICATE* :be-the (or symbol function)

; Initial value is `#<FUNCTION EQUALP>`

;;;; Affected By:

;;;; Notes:

(requirements-about STORE-AS-DEFAULT :doc-type function)

;;;; Description:
; Store TARGET as default-language. Intended to be bound by *break-on-missing*.

#+syntax (STORE-AS-DEFAULT TARGET) ; => result

;;;; Arguments and Values:

; target := (or string symbol), otherwise signals implementation dependent condition.
#?(store-as-default '(not symbol or string)) :signals condition
,:lazy t

; result := TARGET

;;;; Affected By:

;;;; Side-Effects:
; Current language dictionary is modified.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DELETE-DICTIONARY :doc-type function)

;;;; Description:

#+syntax (DELETE-DICTIONARY LANGUAGE) ; => result

;;;; Arguments and Values:

; language := keyword.

; result := boolean.
; Return T if actualy removed, otherwise NIL.
#?(delete-dictionary :test) => T
#?(delete-dictionary :test) => NIL

;;;; Affected By:
; localizer::*dictionaries*

;;;; Side-Effects:
; Modify localizer::*dictionaries*

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LACK-MIDDLEWARE :doc-type function)

;;;; Description:

#+syntax (LACK-MIDDLEWARE APP) ; => result

;;;; Arguments and Values:

; app := function

; result := function

;;;; Affected By:
; localizer::*dictionaries*

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PARSE-ACCEPT-LANGUAGE :doc-type function)

;;;; Description:
; Return list of LANGUAGEs that is sorted by great to less quality order.
#?(parse-accept-language "en-US,en;q=0.9,ja;q=0.7")
=> (:EN-US :EN :JA)
,:test equal

#+syntax (PARSE-ACCEPT-LANGUAGE ACCEPT-LANGUAGE) ; => result

;;;; Arguments and Values:

; accept-language := string

; result := list

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

