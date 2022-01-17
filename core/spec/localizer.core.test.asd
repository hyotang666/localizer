; vim: ft=lisp et
(in-package :asdf)
(defsystem "localizer.core.test"
  :version
  "0.1.0"
  :depends-on
  (:jingoh "localizer.core")
  :components
  ((:file "localizer.core"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :localizer.core args)))
