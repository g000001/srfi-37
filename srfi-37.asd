;;;; srfi-37.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-37
  :version "20200402"
  :description "SRFI 37: args-fold: a program argument processor"
  :long-description "SRFI 37: args-fold: a program argument processor
https://srfi.schemers.org/srfi-37"
  :author "Anthony Carrico"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (fiveam
               srfi-5
               srfi-9
               srfi-11
               srfi-61)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-37")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-37))))
  (let ((name "https://github.com/g000001/srfi-37")
        (nickname :srfi-37))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-37))))
  (let ((*package*
         (find-package "https://github.com/g000001/srfi-37#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-37)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
