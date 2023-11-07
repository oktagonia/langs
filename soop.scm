(use-modules (srfi srfi-1))
(use-modules (ice-9 textual-ports))
(define rest cdr)

; TODO: Get varargs workings
; TODO: Clean this stuff up.

(define (evaluate exp stack)
  (cond ((symbol? exp)
         (get-variable-value exp stack))
        ((atomic? exp)
         exp)
        ((quoted? exp)
         (second exp))
        ((cond-statement? exp)
         (eval-cond (rest exp) stack))
        ((eq? (first exp) 'if)
         (if (evaluate (second exp) stack)
             (evaluate (third exp) stack)
             (evaluate (fourth exp) stack)))
        ((assignment? exp)
         (set-variable-value!
          (second exp)
          (evaluate (third exp) stack)
          stack)
         'OK)
        ((definition? exp)
         (set-variable-in-frame!
          (second exp)
          (evaluate (third exp) stack)
          (first stack))
         'OK)
        ((begin? exp)
         (eval-block (rest exp) (cons (make-frame) stack)))
        ((equal? exp '(interaction-environment)) stack)
        ((kappa? exp)
         (make-macro (second exp) (rest (rest exp)) stack))
        ((lambda? exp)
         (make-function (second exp) (rest (rest exp)) stack))
        ((application? exp)
         (funcall (evaluate (first exp) stack) (rest exp) stack))))

(define (funcall applicable arguments stack)
  (cond ((procedure? applicable)
         (apply applicable (eval-list arguments stack)))
        ((function? applicable)
         (transform applicable (eval-list arguments stack)))
        ((macro? applicable)
         (evaluate (transform applicable arguments) stack))))

(define (eval-list seq stack)
  (map (lambda (x) (evaluate x stack)) seq))

(define (application? exp)
  (list? exp))

(define (atomic? exp)
  (or (number? exp)
      (boolean? exp)
      (string? exp)
      (symbol? exp)
      (function? exp)
      (procedure? exp)
      (primitive? exp)))

;;; Quotes

(define (quoted? exp)
  (eq? (first exp) 'quote))

;;; Blocks

(define (begin? exp)
  (eq? (first exp) 'begin))

(define (eval-block block stack)
  (if (= 1 (length block))
      (evaluate (first block) stack)
      (begin (evaluate (first block) stack)
             (eval-block (rest block) stack))))

;;; Conditionals

(define (cond-statement? exp)
  (eq? (first exp) 'cond))

(define (eval-cond exp stack)
  (let ((clause (first exp)))
    (cond ((eq? (first clause) 'else)
           (evaluate (second clause) stack))
          ((evaluate (first clause) stack)
           (evaluate (second clause) stack))
          (else (eval-cond (rest exp) stack)))))

;;; Functions and macros

(define (kappa? exp)
  (eq? (first exp) 'kappa))

(define (macro? object)
  (eq? (first object) 'macro))

(define (make-macro parameters body stack)
  (list 'macro parameters body stack))

(define (lambda? exp)
  (eq? (first exp) 'lambda))

(define (function? object)
  (eq? (first object) 'function))

(define (make-function parameters body stack)
  (list 'function parameters body stack))

(define app-params second)
(define app-body third)
(define app-stack fourth)

(define (proper lst)
  (let loop ((acc '()) (l lst))
    (if (not (pair? l))
        acc
        (loop (append acc (list (car l))) (cdr l)))))

(define (dotted lst)
  (if (null? lst)
      '()
      (let ((item (cdr (last-pair lst))))
        (if (null? item)
            item
            (list item)))))

(define (transform applicable arguments)
  (let* ((params (app-params applicable))
         (stack (app-stack applicable))
         (arity (length (proper params))))
    (eval-block
     (app-body applicable)
     (extend
      stack
      (append (proper params) (dotted params))
      (append (take arguments arity)
              (list (drop arguments arity)))))))

(define (load-file file)
  (eval-list
    (read
      (open-input-string
        (string-append "(" (get-string-all (open-input-file file)) ")")))
   *standard-stack*))

(define *primitive-functions*
  (let ((table (make-hash-table)))
    (hash-set! table '+ +)
    (hash-set! table '- -)
    (hash-set! table 'append append)
    (hash-set! table '* *)
    (hash-set! table '/ /)
    (hash-set! table 'eq? eq?)
    (hash-set! table 'symbol? symbol?)
    (hash-set! table 'atomic? atomic?)
    (hash-set! table 'null? null?)
    (hash-set! table 'equal? equal?)
    (hash-set! table 'eval evaluate)
    (hash-set! table 'apply funcall)
    (hash-set! table 'or (lambda (x y) (or x y)))
    (hash-set! table 'and (lambda (x y) (and x y)))
    (hash-set! table 'not (lambda (x) (not x y)))
    (hash-set! table '= =)
    (hash-set! table '< <)
    (hash-set! table '> >)
    (hash-set! table 'cons cons)
    (hash-set! table 'car car)
    (hash-set! table 'cdr cdr)
    (hash-set! table 'list list)
    (hash-set! table 'load load-file)
    table))

(define (primitive? function)
  (and (hash-ref *primitive-functions* function) #t))

;;; Mutable state

(define (definition? exp)
  (eq? (first exp) 'define))

(define (assignment? exp)
  (eq? (first exp) 'set!))

(define (make-frame)
  (make-hash-table))

(define (set-variable-in-frame! variable value frame)
  (hash-set! frame variable value))

(define (get-variable-in-frame variable frame)
  (hash-ref frame variable))

(define make-stack list)

(define (get-variable-value variable stack)
  (if (null? stack)
      (error "Undefined variable" variable)
      (or (get-variable-in-frame variable (first stack))
          (get-variable-value variable (rest stack)))))

(define (set-variable-value! variable value stack)
  (set-variable-in-frame! variable value (first stack)))

(define (set-variable-value! variable value stack)
  (if (get-variable-in-frame variable (first stack))
      (set-variable-in-frame! variable value (first stack))
      (set-variable-value! variable value (rest stack))))

(define (extend stack parameters arguments)
  (cons (let loop ((frame (make-frame))
                   (params parameters)
                   (args arguments))
          (if (null? params)
              frame
              (begin
                (set-variable-in-frame!
                 (first params)
                 (first args)
                 frame)
                (loop frame (rest params) (rest args)))))
        stack))

(define *standard-stack*
  (make-stack *primitive-functions*))

;;; User interface

(define (repl)
  (display ">")
  (display (evaluate (read) *standard-stack*))
  (newline)
  (repl))

(load-file "base.soop")
(repl)
