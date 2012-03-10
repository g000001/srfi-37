;;;; srfi-37.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-37
  :serial t
  :depends-on (:fiveam
               :srfi-9
               :srfi-11)
  :components ((:file "package")
               (:file "srfi-37")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-37))))
  (load-system :srfi-37)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-37.internal :srfi-37))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
