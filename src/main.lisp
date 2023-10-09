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

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (car list) x)))

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

;; マクロかどうか？だけではなく、マクロ自身の関数を返す。
(defun scheme-macro (symbol)
  (and (symbolp symbol)
       ;; シンボルのプロパティにせず別管理にしたほうがよい？
       (get symbol 'scheme-macro)))

;; eval-whenは必要？
;; シンボルのshcheme-marco属性に関数を格納する。
(defmacro def-scheme-macro (name paramlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme-macro)
         #'(lambda ,paramlist ,@body)))

(defun scheme-macro-expand (expr)
  "Macro expand this Scheme expression."
  (if (and (listp expr) (scheme-macro (car expr)))
      (scheme-macro-expand
       (apply (scheme-macro (car expr)) (rest expr)))
      expr))

(def-scheme-macro let (bindings &rest body)
  `((lambda ,(mapcar #'car bindings) ,@body)
    ,@(mapcar #'cadr bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 't)
        ((length=1 args) (car args))
        (t `(if ,(car args)
                (and ,@(cdr args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (car args))
        ;; orは成立したときの値を返す。
        (t (let ((var (gensym)))
             `(let ((,var ,(car args)))
                (if ,var ,var
                    (or ,@(cdr args))))))))

;; (cond
;;   ((= a 10) :a10)
;;   ((= a 20) :a20)
;;   (t :otherwise))
;; Schemeの場合else
(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (car clauses))
         `(or ,(car clauses) (cond ,@(cdr clauses))))
        ((starts-with (car clauses) 'else)
         `(begin ,@(cdr (car clauses))))
        (t `(if ,(car (car clauses))
                (begin ,@(cdr (car clauses)))
                (cond ,@(cdr clauses))))))

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
      (cond ,(mapcar
              #'(lambda (clause)
                  (if (starts-with clause 'else)
                      clause
                      ;; eqv?だと思ったが、clauseは ((a b c) :a_b_c) のようにリストを書くようだ。
                      ;; Common Lispだと ((a) :a) → (a :a) と書ける。
                      ;; mklistを挟むと良いのかもしれない。
                      `((member ,key-val ',(car clause))
                        ,@(cdr clause))))
              clauses)))))

(def-scheme-macro define (name &rest body)
  ;; symbolpではない？
  (if (atom name)
      `(begin (set! ,name ,@body) ,name)
      `(define ,(car name)
           (lambda ,(cdr name) ,@body))))

(def-scheme-macro delay (computation)
  `(lambda () ,computation))

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (car v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! ,@v)) bindings)
     ,@body))

(defstruct (proc (:print-function print-proc))
  "Represent a Scheme procedure"
  code (env nil) (name nil) (params nil))

(defun print-proc (proc &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "PROC{~a}" (or (proc-name proc) '??)))

;; tail recursion optimize
(defun interp (expr &optional env)
  "Interpret(evaluate) the expression expr in the envrionemt env."
  ;; (format t "interp expr=~a env=~a~%" expr env)
  (prog ()
     :INTERP
     (return
       (cond
         ((symbolp expr) (get-var expr env))
         ((atom expr) expr)
         ((scheme-macro (car expr))
          (let ((expand-expr (scheme-macro-expand expr)))
            (format t "macro-expand: befor ~a~%" expr)
            (format t "macro-expand: after ~a~%" expand-expr)
            (setf expr expand-expr)
            (go :INTERP)))
         ((case (car expr)
            (quote (cadr expr))
            (begin
             ;; beginを取り除く
             (pop expr)
             ;; 最後の式の一つ手前まで評価する。
             (loop while (cdr expr)
                   do (interp (pop expr) env))
             ;; 最後の式をexprに設定して最初へ
             (setf expr (car expr))
             (go :INTERP))
            (set! (set-var! (cadr expr) (interp (caddr expr) env) env))
            (if (setf expr
                      (if (interp (cadr expr) env)
                          (caddr expr)
                          (cadddr expr)))
                (go :INTERP))
            (lambda
                (make-proc :name "lambda" :params (cadr expr) :env env
                           :code (maybe-add 'begin (cddr expr))))
            ;; 手続き適用
            (t (let ((proc (interp (car expr) env))
                     (args (mapcar #'(lambda (expr2) (interp expr2 env)) (cdr expr))))
                 (if (proc-p proc)
                     (progn
                       (setf expr (proc-code proc))
                       (setf env (extend-env (proc-params proc) args (proc-env proc)))
                       (go :INTERP))
                     (apply proc args))))))))))

(defun interp-no-tail-recursion-optimize (expr &optional env)
  "Interpret(evaluate) the expression expr in the envrionemt env."
  ;; (format t "interp expr=~a env=~a~%" expr env)
  (cond
    ((symbolp expr) (get-var expr env))
    ((atom expr) expr)
    ((scheme-macro (car expr))
     (let ((expand-expr (scheme-macro-expand expr)))
       ;; (format t "macro-expand: befor ~a~%" expr)
       ;; (format t "macro-expand: after ~a~%" expand-expr)
       (interp expand-expr env)))
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

;; for test
;; (interp '(define (traverse lst) (if lst (traverse (cdr lst))))' nil)
