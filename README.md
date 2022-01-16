# LOCALIZER 0.0.0
## What is this?
The system to localize your program text messages.

## Alternatives and differences.
* [gettext]
* [translate]
* [acclimation]
* [cl-i18n]
* [cl-l10n]
* [cl-locale]

```lisp
(cond
  ((want-to-ship-your-app-as-compiled-one?)
   (if (familier-with-gnu-gettext?)
       "gettext may better"
       "translate may better"))
  ((especially-for-error-message-of-your-lisp-library?)
   "acclimation may better")
  ((famillier-with-gnu-gettext?)
   (or "cl-i10n" "cl-l10n"))
  ((or (want-template?) (not (use-only-literal-string?)) (want-lack-middleware?))
   "Welcome to localizer!")
  (t (or "cl-locale" "translate")))
```

### Related works.

* [language-codes]

# Usage
## Step 1.
Writing your lisp program.

```lisp
(defun hello (to)
  (format t "Hello ~S!" to))
```

## Step 2.
Setup default language. (`:EN` is the default.)

```lisp
(setq localizer:*default-language* :en)
```

## Step 3.
Wrap your text with `localizer:localize`.

```lisp
(defun hello (to)
  (format t (localizer:localize "Hello ~S!") to))
```

## Step 4.
Generate a template for a new language.

```lisp
(localizer:template :ja)

(LOCALIZER:DEFDICT :JA     ; <--- Side effect by cl:print.
  "Hello ~S!" "Hello ~S!")
(LOCALIZER:DEFDICT :JA     ; <--- The return value.
  "Hello ~S!" "Hello ~S!")
```

Save it as an ordinary lisp file. (Do not forget to update asd file if it is a new file.)

## Step 5.
Edit it.

```lisp
(LOCALIZER:DEFDICT :JA
  "Hello ~S!" "おはようございます~Sさん。")
```

## Step 6.
Finally, you can run your code with language switching via the special variable `localizer:*language*`.

```lisp
(hello 'sato) ; A case to use the default language :EN.
Hello SATO!
NIL

(let ((localizer:*language* :ja))
  (hello 'sato))
おはようございますSATOさん。
NIL
```

## READER-MACRO.
Localizer provides reader-macro.

You can use it in two ways.

* Evaluate `localizer:set-syntax`.

```lisp
(localizer:set-syntax)
```

The default dispatch sub-character is `#\l`.
Of course, you can specify any character if you want.

```lisp
(localizer:set-syntax #\t)
```

* Use [named-readtables] with `localizer:|#L-reader|`.

```lisp
(named-readtables:defreadtable my-table
  (:merge :standard)
  (:dispatch-macro-char #\# #\l 'localizer:|#L-reader|))

(named-readtables:in-readtable my-table)
```

After that, you can use reader macro instead of wrapping the text.

```lisp
(defun hello (to)
  (format t #L"Hello ~S!" to))
```

Reader macro is useful especially for minimizing the edit.

### *NOTE!*

Unlike standard common-lisp reader macros, `localizer:|#L-reader|` generates the form, not an object.
So reader macro does not work expectedly in literal object form.

```lisp
'(#L"not localized") => ((LOCALIZER:LOCALIZE "not localized"))
```

## Limitations.
Under the hood, `localizer:localize` has a compiler macro.
And it responds to collect the default text messages in the compile time.
So `localizer:template` generates the form without any key-value pairs if it is before compiling.

Additionally, collecting the default text messages in the compile-time occurs only when an argument is a literal object.
So if it is a dynamic one, such keys are not collected by compiler macro.

This means locazier supports dynamic translations.
This is useful and efficient if one of the words will be chosen.

```lisp
#L(aref #("Hi" "Hello" "Hey") (random 3))

;; Instead of
(aref (vector #L"Hi" #L"Hello" #L"Hey") (random 3))
```

Later case above translates three words and allocates vector in every call
but the first case translates one word and no vector allocation occurs in the run time.

You may notice the sentence about 'argument is a literal *object*'.
Yes, in fact, `STRING` or `SYMBOL` is able to be the key.
This is useful when the text message is too much longer, or to avoid key conflict.

You may also notice the sentence about 'collecting the default text messages *in the compile-time*'.
Yes, in fact, collecting may occur in run time too. (The default.)
If the word that does not exist in the dictionary of the current language comes,
localizer `cl:funcall`s the function designator in the special variable `localizer:*break-on-missing*` with such a word.
The default value of the `localizer:*break-on-missing*` is `localizer:store-as-default`.
If you want to skip storing, bind it with `cl:identity`.

In the developing phase, you may want to get warnings.
In such cases, you can do it like below.

```lisp
(setq localizer:*break-on-missing*
  (lambda (word)
    (warn "Unknown word is found! ~S" word)
    (localizer:store-as-default word)))
```

## `localizer:defdict`
The macro `localizer:defdict` defines a dictionary for a new language.

The first argument is a keyword symbol to name a new language.

Rest arguments are key-value pairs.

## `localizer:lack-middleware`
Localizer provides [lack] middleware `localizer:lack-middleware`.

It parses accept-language header and detects supported language then binds `localizer:*language*` automatically.

```lisp
(lack:builder #'localizer:lack-middleware <your-app>)
```

## From developer

### Product's goal

### License

### Developed with
* SBCL

### Tested with
* SBCL/2.2.0
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* allegro/10.1
* abcl/1.8.0
* cmucl/21D

## Installation

<!-- Links -->

[translate]:https://gitlab.common-lisp.net/dkochmanski/translate
[language-codes]:https://github.com/Shinmera/language-codes
[gettext]:https://github.com/rotatef/gettext
[cl-peppol]:https://github.com/mmontone/cl-peppol
[cl-locale]:https://github.com/fukamachi/cl-locale
[cl-l10n]:https://gitlab.common-lisp.net/cl-l10n/cl-l10n
[cl-l10n-cldr]:https://gitlab.common-lisp.net/cl-l10n/cl-l10n-cldr
[acclimation]:https://github.com/robert-strandh/Acclimation
[cl-i18n]:https://notabug.org/cage/cl-i18n.git
[gnu gettext]:https://www.gnu.org/software/gettext/
[named-readtables]:https://github.com/melisgl/named-readtables
[lack]:https://github.com/fukamachi/lack
