(defpackage mini-scheme
  (:use :cl))
(in-package :mini-scheme)

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (car (last list)))

(defvar *global-env* nil)

(defun get-global-var (var)
  (let ((pair (assoc var *global-env*)))
    (if (null pair)
        (error "Unbound scheme variable: ~a" var)
        (cdr pair))))

(defun set-global-var! (var val)
  (if (assoc var *global-env*)
      (setf (cdr (assoc var *global-env*)) val)
      (setf *global-env* (acons var val *global-env*)))
  val)

(defun get-var (var env)
  "Get the value of a variable, from the given or global envrionemt"
  ;; TODO if-letを使いたい。
  (if (assoc var env)
      (cdr (assoc var env))
      (get-global-var var)))

(defun set-var! (var val env)
  (if (assoc var env)
      (setf (cdr (assoc var env)) val)
      (set-global-var! var val))
  val)

(defun length=1 (list)
  "Is list a list of length 1?"
  (and (consp list) (null (cdr list))))

(defun last1 (list)
  (car (last list)))

(defun maybe-add (op exprs &optional if-nil)
  "For example, (maybe-add 'and exprs t) returns
t if exprs is nil, exps if there is only one,
and (and expr1 expr2...) if there are several exprs."
  (cond ((null exprs) if-nil)
        ((length=1 exprs) (car exprs))
        (t (cons op exprs))))

;; SICPのように varとvalのリストが連鎖する構成になっていない。
(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  ;; nconcは後ろのリストは変更されないので、これで問題なし。
  (nconc (mapcar #'cons vars vals) env))

;; TODO 正しい構文かチェックする。
(defun interp (expr &optional env)
  "Interpret(evaluate) the expression expr in the envrionemt env."
  ;; (format t "interp expr=~a env=~a~%" expr env)
  (cond
    ((symbolp expr) (get-var expr env))
    ((atom expr) expr)
    ((case (car expr)
       ;; TODO cadrで正しい？
       (quote (cadr expr))
       (begin (last1 (mapcar #'(lambda (expr2) (interp expr2 env)) (cdr expr))))
       (set! (set-var! (cadr expr) (interp (caddr expr) env) env))
       (if (if (interp (cadr expr) env)
               (interp (caddr expr) env)
               (interp (cadddr expr) env)))
       (lambda (let ((params (cadr expr))
                     (code (maybe-add 'begin (cddr expr))))
                 ;; (format t "lambda params=~a~%" params)
                 ;; (format t "lambda code=~a~%" code)
                 #'(lambda (&rest args)
                     ;; (format t "lambda args=~a~%" args)
                     (interp code (extend-env params args env)))))
       (t (apply (interp (car expr) env)
                 (mapcar #'(lambda (expr2) (interp expr2 env)) (cdr expr))))))))

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-scheme-proc (f)
  "Define a Scheme procedure as a corresponding CL functions."
  (if (listp f)
      (set-global-var! (car f) (symbol-function (cadr f)))
      (set-global-var! f (symbol-function f))))

(defun init-scheme-interp ()
  "Initialize the scheme interpreter with some global variables."
  (dolist (f *scheme-procs*)
    (init-scheme-proc f))
  (set-global-var! t t)
  (set-global-var! nil nil))

(defun scheme (&optional expr)
  "A Scheme read-eval-print loop (using interp)"
  (init-scheme-interp)
  (loop
    (format t "~&==> ")
    (print (interp (read) nil))))

(defun test1 ()
  (set-global-var! 'nil nil)
  (set-global-var! '+ #'+))

(defun test2 ()
  (set! fact
      (lambda (n)
        (if (= n 0) 1
            (* n (fact (- n 1))))))
  (set! table
        (lambda (f start end)
          (if (<= start end)
              (begin
               (write (list start (f start)))
               (newline)
               (table f (+ start 1) end))))))

