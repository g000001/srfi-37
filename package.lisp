;;;; package.lisp

(cl:in-package common-lisp-user)


(defpackage "https://github.com/g000001/srfi-37"
  (:use)
  (:export
   args-fold
   option
   option-names
   option-required-arg?
   option-optional-arg?
   option-processor ))


(defpackage "https://github.com/g000001/srfi-37#internals"
  (:use
   "https://github.com/g000001/srfi-37"
   "https://github.com/g000001/srfi-9"
   "https://github.com/g000001/srfi-5"
   "https://github.com/g000001/srfi-61"
   "https://github.com/g000001/srfi-11"
   cl
   fiveam)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-5"
   let)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-61"
   cond => else)
  (:shadow lambda member map assoc write loop find))


;;; *EOF*
