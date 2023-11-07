(use-modules (ice-9 hash-table))

;;; Streams

(define (memoized-procedure p)
  (let ((run? #f) (result #f))
    (lambda ()
      (if run?
          result
          (begin
            (set! run? #t)
            (set! result (p))
            result)))))

(define-macro (delay M)
  `(memoized-procedure (lambda () ,M)))

(define (force M)
  (M))

(define (body M)
  (caddr M))

(define (params M)
  (cadr M))

(define (get-env M)
  (cadddr M))

(define (make-function params body env)
  (list 'λ params body env))

;;; Lazy lambda-calculus evaluator (without beta-reduction)

(define (church-numeral x)
  (if (= x 0)
      'o
      `(s ,(church-numeral (- x 1)))))

(define (evaluate exp env)
  (cond ((symbol? exp) (force (hash-ref env exp)))
        ((number? exp) (evaluate `(lambda (s) (lambda (o) ,(church-numeral exp))) env))
        ((eq? (car exp) 'quote)
         (list-ref exp 1))
        ((or (eq? (car exp) 'define)
             (eq? (car exp) 'set!))
         (hash-set!
          env
          (list-ref exp 1)
          (delay (evaluate (list-ref exp 2) env))))
        ((eq? (car exp) 'lambda) (make-function (params exp) (body exp) env))
        ((eq? (car exp) 'eq?)
         (eq? (evaluate (cadr exp) env) (evaluate (caddr exp) env)))
        (else
         (call (evaluate (car exp) env)
               (map (lambda (x) (delay (evaluate x env))) (cdr exp))))))

(define (call fn args)
  (evaluate
   (body fn)
   (extend (get-env fn) (params fn) args)))

(define (extend env params args)
  (alist->hash-table
   (append (hash-map->list cons env)
           (map cons params args))))

;;; Repl

(define (print s)
  (display s) ; (if (symbol? s) s (stream->list s)))
  (newline))

(define-macro (loop M)
  `(let loop ()
     ,M
     (loop)))

(define stdenv
  (let ((env (make-hash-table)))
    (evaluate '(define true? (lambda (f) (eq? ((f 'a) 'b) 'a))) env)
    (evaluate '(define zero? (lambda (n) (lambda (x) (lambda (y) ((n (lambda (x) y)) x))))) env)
    (evaluate '(define O (lambda (s) (lambda (o) o))) env) ; zero
    ; S(x) = x + 1
    (evaluate '(define S (lambda (f) (lambda (m) (lambda (x) (m ((f m) x)))))) env)
    ; P(x) = x - 1
    (evaluate '(define P
                 (lambda (n)
                   (lambda (f)
                     (lambda (x)
                       (((n (lambda (g) (lambda (h) (h (g f)))))
                         (lambda (u) x)) (lambda (u) u)))))) env)

    (evaluate '(define * (lambda (n m) (lambda (f) (n (m f))))) env)
    (evaluate '(define + (lambda (n m) (lambda (f) (lambda (x) ((n f) ((m f) x)))))) env)
    (evaluate '(define A (lambda (x) (lambda (y) (y ((x x) y))))) env)
    ; Turing fixed-point combinator. Used for solving recursive equations
    (evaluate '(define Theta (A A)) env)
    ; factorial using Theta
    (evaluate '(define fact
                 (Theta
                  (lambda (f)
                    (lambda (n)
                      (((zero? n) (S O)) (* n (f (P n)))))))) env)
    ; Y combinator
    (evaluate '(define Y (lambda (g) ((lambda (x) (g (x x))) (lambda (x) (g (x x)))))) env)
    env))

(define (repl)
  (loop (print (evaluate (read) stdenv))))

(repl)
