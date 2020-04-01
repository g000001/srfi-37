(cl:in-package "https://github.com/g000001/srfi-37#internals")


(def-suite* srfi-37)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun options () '#:options)
  (defun args () '#:args))

(defvar #.(options)
 (list (option '(#\d "debug") 'NIL 'T
               (lambda (option name arg debug batch paths files)
                 (list option name arg debug batch paths files)
                 (values (or arg "2") batch paths files)))
       (option '(#\b "batch") 'NIL 'NIL
               (cl:lambda (option name arg debug batch paths files)
                 (list option name arg debug batch paths files)
                 (values debug 'T paths files)))
       (option '(#\I "include") 'T 'NIL
               (lambda (option name arg debug batch paths files)
                 (list option name arg debug batch paths files)
                 (values debug batch (cons arg paths) files) ))))


(defvar #.(args) '("prog" "-d1" "-b" "-Ifoo" "-Ibar" "-Ibaz" "f1" "f2" "f3"))


(test srfi-37
  (is (string=
       (with-output-to-string (s)
         (multiple-value-bind (debug-level batch-mode include-paths files)
                              (args-fold (cdr #.(args))
                                         #.(options)
                                         (lambda (option name arg . seeds)
                                           ;; unrecognized
                                           (list option name arg seeds)
                                 (error "Unrecognized option:~A" name) )
                               (lambda (operand debug batch paths files)
                                 ;; operand
                                 (list operand debug batch paths files)
                                 (values debug batch paths (cons operand files)) )
                               0      ; default value of debug level
                               'NIL     ; default value of batch mode
                               '()    ; initial value of include paths
                               '()    ; initial value of files
                               )
           (with-standard-io-syntax
             (format s "debug level = ~A~%" debug-level)
             (format s "batch mode = ~A~%" batch-mode)
             (format s "include paths = ~A~%" (reverse include-paths))
             (format s "files = ~A~%" (reverse files)))
           0))
       "debug level = 1
batch mode = T
include paths = (foo bar baz)
files = (f1 f2 f3)
")))


#|(define (main args)
  (srfi-8:receive (debug-level batch-mode include-paths files)
    (args-fold (cdr args)
               options
               (lambda (option name arg . seeds)         ; unrecognized
                 (error "Unrecognized option:" name))
               (lambda (operand debug batch paths files) ; operand
                 (values debug batch paths (cons operand files)))
               0      ; default value of debug level
               'NIL     ; default value of batch mode
               '()    ; initial value of include paths
               '()    ; initial value of files
               )
     (print "debug level = " debug-level)
     (print "batch mode = " batch-mode)
     (print "include paths = " (reverse include-paths))
     (print "files = " (reverse files))
     0))|#


;;; *EOF*
