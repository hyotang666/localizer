; vim: ft=lisp et
(in-package :asdf)
(defsystem "localizer.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "localizer")
  :components
  ((:file "localizer"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :localizer args)))