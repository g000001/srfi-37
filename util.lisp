(cl:in-package :srfi-37.internal)

(cl:defconstant +eof+
  (if (cl:boundp '+eof+)
      (cl:symbol-value '+eof+)
      (list cl:nil)))

(defmacro defsynonymfun (name fcn)
  `(cl:setf (cl:fdefinition ',name) ,fcn))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (cl:setf (cl:fdefinition 'eq?) #'cl:eq)
    (cl:setf (cl:fdefinition 'null?) #'cl:null)
    (cl:setf (cl:fdefinition 'pair?) #'cl:consp)
    (cl:setf (cl:fdefinition 'zero?) #'cl:zerop)
    (cl:setf (cl:fdefinition 'vector-length) #'cl:length)
    (cl:setf (cl:fdefinition 'vector?) #'cl:vectorp)
    (cl:setf (cl:fdefinition 'procedure?) #'cl:functionp)
    (cl:setf (cl:fdefinition 'newline) #'cl:terpri)
    (cl:setf (cl:fdefinition 'display) #'cl:princ)
    (cl:setf (cl:fdefinition 'string-length)  #'cl:length)
    (cl:setf (cl:fdefinition 'char->integer)  #'cl:char-code)
    (cl:setf (cl:fdefinition 'string-ref) #'cl:char)
    (cl:setf (cl:fdefinition 'symbol->string) #'cl:string)
    (cl:setf (cl:fdefinition 'string?) #'cl:stringp)
    (cl:setf (cl:fdefinition 'symbol?) #'cl:symbolp)
    (cl:setf (cl:fdefinition 'char?) #'cl:characterp)
    (cl:setf (cl:fdefinition 'string=?) #'cl:string=)
    (cl:setf (cl:fdefinition 'string-ci=?) #'cl:string-equal)
    (cl:setf (cl:fdefinition 'map) #'cl:mapcar)
    (cl:setf (cl:fdefinition 'char=?) #'cl:char=)
    (cl:setf (cl:fdefinition 'char<?) #'cl:char<)
    (cl:setf (cl:fdefinition 'char-ci=?) #'cl:char-equal)
    (cl:setf (cl:fdefinition 'char-ci<?) #'cl:char-lessp)
    (cl:setf (cl:fdefinition 'string<?) #'cl:string<)
    (cl:setf (cl:fdefinition 'string-ci<?) #'cl:string-lessp)
    (cl:setf (cl:fdefinition 'real?) #'cl:realp)
    (cl:setf (cl:fdefinition 'exists) #'cl:some)
    ))

(defun list->string (list)
  (coerce list 'string))

(defsynonymfun char-alphabetic? #'cl:alpha-char-p)
(defsynonymfun string->number #'cl:parse-integer)
(defsynonymfun string->symbol #'cl:intern)
(defsynonymfun char-numeric? #'cl:digit-char-p)

(defun list? (obj)
  (cl:and (cl:listp obj)
          (cl:tailp '() obj)))

(defmacro set! (var val)
  `(cl:setq ,var ,val))

(cl:declaim (cl:inline list-tail vector-set! list-ref vector->list list->vector
                       quotient set-car! set-cdr! eqv?
                       assq assv assoc for-each memq))

(defun eqv? (x y)
  (cl:eql x y))

(defun member (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (cl:eql item (car e))
      (cl:return e))))

(defun memq (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (cl:eq item (car e))
      (cl:return e))))


(defun for-each (fn cl:&rest lists)
  (cl:apply #'cl:mapc fn lists)
  nil)

(defun assq (item alist)
  (cl:assoc item alist :test #'eq?))

(defun assv (item alist)
  (cl:assoc item alist :test #'eqv?))

(defun assoc (item alist)
  (cl:assoc item alist :test #'equal?))

(defun equal? (x y)
  (cl:equal x y))

(defun set-car! (list obj)
  (cl:rplaca list obj))

(defun set-cdr! (cons x)
  (cl:rplacd cons x))

(defun list-tail (list k)
  (cl:nthcdr k list))

(defun list-ref (list k)
  (cl:nth k list))

(defun vector-set! (vec index val)
  (cl:setf (cl:aref vec index) val))

(defun string-set! (str index val)
  (cl:setf (cl:char str index) val))

(defun vector->list (vec)
  (cl:coerce vec 'list))

(defun list->vector (list)
  (cl:coerce list 'cl:vector))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (cl:typecase list
      (cl:list (if (cl:tailp () list)
                   list
                   (cl:let ((last (cl:last list)))
                     `(,@(cl:butlast list)
                         ,(car last)
                         cl:&rest
                         ,(cdr last)))))
      (cl:symbol `(cl:&rest ,list)))))

(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body ))

(defmacro letrec ((&rest binds) &body body)
  `(let (,@(cl:mapcar (cl:lambda (x)
                        `(,(car x) #'values) )
             binds ))
     (declare (optimize (space 3)))
     (labels (,@(cl:remove nil
                  (cl:mapcar (cl:lambda (x &aux (name (car x)))
                               `(,name
                                 (&rest args)
                                 (apply ,name args) ))
                             binds )))
       (declare (optimize (debug 0) (space 3)))
       (psetq ,@(cl:apply #'cl:append binds))
       ,@body )))

(defmacro define-function (name-args &body body)
  (if (cl:consp name-args)
      (cl:destructuring-bind (name . args)
           name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body ))
      `(cl:progn
         (cl:eval-when (:compile-toplevel :load-toplevel :execute)
           (cl:setf (cl:fdefinition ',name-args)
                    (cl:function cl:values) ))
         (cl:setf (cl:fdefinition ',name-args)
                  ,(car body) ))))

(cl:declaim (cl:inline vector-ref))
(defun vector-ref (vec k)
  (cl:svref vec k) )

(defmacro begin (&body body)
  `(progn ,@body))

(cl:declaim (cl:inline make-vector))
(defun make-vector (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))

(cl:declaim (cl:inline string-append))
(defun string-append (&rest strings)
  (cl:format nil "~{~A~}" strings))

(cl:defmacro dolex ((&rest varlist) endlist &body body)
  (let* ((vars (cl:mapcar (cl:lambda (v)
                            (if (cl:consp v) (car v) v) )
                          varlist ))
         (binds (cl:mapcar (cl:lambda (b)
                             (if (cl:consp b)
                                 (cl:destructuring-bind (var &optional init next)
                                      b
                                   (if next
                                       `(,var ,init
                                              (let (,@(cl:mapcar (cl:lambda (x)
                                                                   (list x x) )
                                                        vars ))
                                                (declare (ignorable ,@vars))
                                                ,next ))
                                       `(,var ,init) ))
                                 (list b nil) ))
                           varlist )))
    `(cl:do ,binds ,endlist ,@body) ))


(defmacro with-local-define-function (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil name-arg . bo) :in defines
       :collect (cl:let ((name-arg (to-proper-lambda-list name-arg)))
                  `(,(car name-arg) ,(cdr name-arg) ,@bo) )
       :into defs
       :finally (cl:return
                  `(labels (,@defs)
                     ,@body )))))

(defmacro with-local-define-variable (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil v bo) :in defines
       :collect v :into vars
       :collect v :into setqs
       :collect bo :into setqs
       :finally (cl:return
                  `(cl:let (,@vars)
                     (cl:psetq ,@setqs)
                     ,@body )))))

(defun boolean? (obj)
  (cl:typep obj '(cl:member cl:t cl:nil)))


(defun eof-object? (obj)
  (eq? obj +eof+))

(defmacro iterate (tag specs &body body)
  (let ((vars  (map #'car specs))
        (vals  (map #'cl:cadr specs))
	(id    (cl:gensym))
        (dvars (cl:map-into (cl:make-list (cl:length specs)) #'cl:gensym)))
    `(block ,id
       (let ,(map #'list dvars vals)
         (macrolet ((,tag ,vars
                      `(progn (psetq ,@(list ,@(map #'(cl:lambda (dvar var)
                                                           `(',dvar ,var))
                                                       dvars
                                                       vars)))
                              (go ,',id))))
           (tagbody
             ,id
             (let ,(map #'list vars dvars)
               (return-from ,id (progn ,@body)))))))))


;;; eof
