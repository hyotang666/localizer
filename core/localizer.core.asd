; vim: ft=lisp et
(in-package :asdf)
(defsystem "localizer.core"
  :version
  "1.0.1"
  :author "SATO Shinichi"
  :description "Core module for localizer."
  :source-control (:git "git@github.com:hyotang666/localizer")
  :bug-tracker "https://github.com/hyotang666/localizer/issues"
  :license "MIT"
  :depends-on
  (
   "uiop"       ; Utilities, implicitly depends on via asdf.
   )
  :pathname
  "src/"
  :components
  ((:file "localizer.core")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "localizer.core").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "localizer.core"))))
  (append (call-next-method) '((test-op "localizer.core.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "localizer.core")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "localizer.core"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
