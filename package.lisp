;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-37
  (:use)
  (:export))

(defpackage :srfi-37.internal
  (:use :srfi-37 :cl :fiveam :srfi-9 :srfi-11)
  (:shadow :lambda :member :map :assoc :write :loop :find))
