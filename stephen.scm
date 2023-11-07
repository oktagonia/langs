;; A simple pattern matching system in RnRS Scheme.

(define (in? x y)
  (if (atom? y)
      (equal? x y)
      (or (in? x (car y)) (in? x (cdr y)))))

(define (index-of obj lst)
  (let loop ((l lst) (i 0))
    (cond ((null? l) #f)
          ((equal? (car l) obj) i)
          (else (loop (cdr l) (1+ i))))))

(define-macro (nambda parameters body)
  `(list 'nambda ',parameters ',body))

(define (nambda? obj)
  (and (list? obj) (eq? (car obj) 'nambda)))

(define nambda-params cadr)
(define nambda-body caddr)

(define (apply-nambda nambda args)
  (let apply ((params (nambda-params nambda))
              (body (nambda-body nambda)))
    (let ((index (index-of body params)))
      (cond (index (list-ref args index))
            ((atom? body) body)
            (else (cons (apply params (car body))
                        (apply params (cdr body))))))))

(define (var? sym)
  (and (symbol? sym)
       (string=? (string-take-right (symbol->string sym) 1) "_")))

(define (variable->symbol var)
    (string->symbol (string-drop-right (symbol->string var) 1)))

(define (variables pattern)
  (cond ((var? pattern) (list pattern))
        ((atom? pattern) '())
        (else (append (variables (car pattern))
                      (variables (cdr pattern))))))

(define (atom? obj) (not (pair? obj)))

(define (match? pattern1 pattern2)
  (cond ((var? pattern1)
         (not (var? pattern2)))
        ((var? pattern2)
         (not (var? pattern1)))
        ((atom? pattern1)
         (or (equal? pattern1 pattern2) (var? pattern2)))
        ((atom? pattern2)
         (or (equal? pattern1 pattern2) (var? pattern1)))
        (else
         (and (match? (car pattern1) (car pattern2))
              (match? (cdr pattern1) (cdr pattern2))))))

(define (resolve pattern1 pattern2 variable)
  (cond ((not (match? pattern1 pattern2))
         (error "Patterns do not match"))
        ((eq? variable pattern1)
         pattern2)
        ((eq? variable pattern2)
         pattern1)
        ((or (in? variable (car pattern1)) (in? variable (car pattern2)))
         (resolve (car pattern1) (car pattern2) variable))
        (else (resolve (cdr pattern1) (cdr pattern2) variable))))

(define-macro (rule pattern replacement)
  `(list '*rule*
    (quote ,pattern)
    (nambda ,(map variable->symbol (variables pattern))
      ,replacement)))

(define rule-pattern cadr)
(define rule-replacement caddr)

(define (apply-rule expression rule)
  (cond ((match? (rule-pattern rule) expression)
         (apply-nambda
          (rule-replacement rule)
          (map
           (lambda (var)
             (resolve (rule-pattern rule) expression var))
           (variables (rule-pattern rule)))))
        ((atom? expression) expression)
        (else (cons (apply-rule (car expression) rule)
                    (apply-rule (cdr expression) rule)))))

(define (apply-all-rules expression rules)
  (let loop ((e expression))
    (let ((matching-rules
           (filter (lambda (r) (match? (rule-pattern r) e)) rules)))
      (if (null? matching-rules)
          e
          (loop (apply-rule e (car matching-rules)))))))

(define evaluation-rules
  (list (rule (car (x_ . y_)) x)
        (rule (cdr (x_ . y_)) y)))

(apply-all-rules '(car (cdr ((x . x) . (y . x)))) evaluation-rules)

(filter (lambda (r) (match? (rule-pattern r) '(quote x))) evaluation-rules)

;; ah fuck it I'm bored.
