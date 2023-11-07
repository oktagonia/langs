(defpackage lisp (:use :cl))

(in-package lisp)

(shadow 'eval)
(shadow 'apply)

;;; Eval and apply

(defun eval (exp env)
  (cond ((symbolp exp) (get-variable env exp))
        ((atomic-p exp) exp)
        ((eq (first exp) 'quote) (second exp))
        ((eq (first exp) 'if)
         (if (eval (second exp) env)
             (eval (third exp) env)
             (eval (fourth exp) env)))
        ((eq (first exp) 'define)
         (define-variable env (second exp) (eval (third exp) env)))
        ((eq (first exp) 'set!)
         (set-variable env (second exp) (eval (third exp) env)))
        ((eq (first exp) 'begin)
         (let ((e (cons (make-frame '()) env)))
           (first (last (mapcar (lambda (x) (eval x e)) (rest exp))))))
        ((eq (first exp) 'lambda)
         (make-procedure (second exp) (third exp) env))
        ((eq (first exp) 'macro)
         (make-macro (second exp) (third exp) env))
        (t (apply (eval (first exp) env) (rest exp) env))))

(defun apply (procedure arguments env)
  (cond ((functionp procedure)
         (cl::apply procedure (mapcar (lambda (x) (eval x env)) arguments)))
        ((macro-p procedure)
         (eval
          (eval
           (body procedure)
           (extend (env procedure) (parameters procedure) arguments))
          env))
        ((procedure-p procedure)
         (eval
          (body procedure)
          (extend (env procedure)
                  (parameters procedure)
                  (mapcar (lambda (x) (eval x env)) arguments))))))

(defun atomic-p (object)
  (not (consp object)))

;;; Procedures and macros

(defun make-procedure (lambda-list body env)
  (vector 'procedure lambda-list body env))

(defun make-macro (lambda-list body env)
  (vector 'syntax lambda-list body env))

(defun macro-p (object)
  (eq (elt object 0) 'syntax))

(defun procedure-p (object)
  (eq (elt object 0) 'procedure))

(defun parameters (procedure)
  (elt procedure 1))

(defun body (procedure)
  (elt procedure 2))

(defun env (procedure)
  (elt procedure 3))

;;; Environment

(defun make-frame (bindings)
  (let ((frame (make-hash-table)))
    (loop :for binding :in bindings
          :do (setf (gethash (first binding) frame) (second binding)))
    frame))

(defun get-variable-in-frame (frame name)
  (gethash name frame))

(defun set-variable-in-frame (frame name value)
  (setf (gethash name frame) value))

(defun make-environment ()
  (list (make-frame '())))

(defun get-variable (environment name)
  (loop :for frame :in environment
        :do (when (get-variable-in-frame frame name)
              (return-from
               get-variable
                (get-variable-in-frame frame name))))
  (error "Undefined variable"))

(defun define-variable (environment name value)
  (set-variable-in-frame (first environment) name value)
  'ok)

(defun set-variable (environment name value)
  (loop :for frame :in environment
        :do (when (get-variable-in-frame frame name)
              (set-variable-in-frame frame name value)
              (return-from set-variable 'ok)))
  (error "Undefined variable"))

(defun extend (environment names values)
  (cons (make-frame (mapcar #'list names values)) environment))

(defun (setup-environment)
  (let ((env (make-environment)))
    (define-variable env 'cons #'cons)
    (define-variable env 'car #'car)
    (define-variable env 'cdr #'cdr)
    (define-variable env '+ #'+)
    (define-variable env '- #'-)
    (define-variable env '* #'*)
    (define-variable env '/ #'/)
    (define-variable env '< #'<)
    (define-variable env '> #'>)
    (define-variable env '= #'=)
    (define-variable env 'eq? #'eq)
    (define-variable env 'eval #'eval)
    (define-variable env 'apply #'apply)
    (define-variable env 'format #'format)
    ; insert whatever functions you want here
    env))

(defparameter *standard-environment* (setup-environment))

;;; Repl

(defun println (object)
  (princ object)
  (format t "~%"))

(defun repl ()
  (loop (println (eval (read) *standard-environment*))))
