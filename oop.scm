(define (variable-bindings bindings)
  (map (lambda (c) (list (car c) (cadr c)))
       (filter (lambda (binding) (symbol? (car binding))) bindings)))

(define (method-bindings bindings)
  (filter (lambda (binding) (pair? (car binding))) bindings))

(define (setup method msg)
  (let* ((name (caar method))
         (args (cdar method))
         (body (cdr method))
         (arity (length args)))
    `((,name)
      (let ,(map (lambda (name index)
                   (list name `(list-ref ,msg ,index)))
                 args
                 (iota arity 1))
        ,@body))))

(define (setup-methods methods msg)
  (let loop ((m methods) (out '()))
    (if (null? m)
        out
        (loop (cdr m)
              (cons (setup (car m) msg) out)))))

(define (*fundumental* . msg)
  (error "No such message exists" msg))

(define-macro (make-object default . bindings)
  (let ((msg (gensym))
        (variables (variable-bindings bindings))
        (methods (method-bindings bindings)))
    `(let ,variables
       (define (self . ,msg)
         (case (car ,msg)
           ,@(setup-methods methods msg)
           (else (apply ,default ,msg))))
       self)))

(define-macro (make-class
               super
               class-vars class-methods
               instance-vars instance-methods)
  `(make-object
    ,super
    (super ,super)
    (variables (quote ,instance-vars))
    (methods (quote ,instance-methods))
    ,@class-vars
    ,@class-methods
    ((new)
     (let ((s (if super (super 'new) *fundumental*)))
       (eval
        (append
         (list 'make-object s)
         (cons (list 'super s)
               (map (lambda (x) (list x #f)) variables))
         methods)
        (interaction-environment))))))
       
(define Object
  (make-class #f
    () ; class variables
    [((subclass class-vars class-methods instance-vars instance-methods)
      (eval (list 'make-class self class-vars class-methods
                  instance-vars instance-methods)
            (interaction-environment)))]
    () ; instance variables
    () ; instance methods
  ))

(define Rectangle
  (Object 'subclass
    '()
    '()
    '(width height)
    '[((area) (* width height))
      ((set-width! new-value) (set! width new-value))
      ((set-height! new-value) (set! height new-value))
      ((type) "Rectangle")]
  ))

(define Square
  (Rectangle 'subclass
    '()
    '()
    '()
    '[((type) "Square")
      ((test) super)
      ((new side)
       (super 'set-width! side)
       (super 'set-height! side))]
  ))

(((Square 'new) 'test) 'type)

#|
Make a Class class, it's instances are classes. Make an Object class
that every object inherits from.
|#
