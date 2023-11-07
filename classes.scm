(use-modules (srfi srfi-1))

(define (constructor-arguments constructor)
  (cadr constructor))

(define (setup-constructor constructor methods)
  (let [(method-names (map caadr methods))
        (method-args (map cdadr methods))]
    `((set! self
            (lambda msg
              (case (car msg)
                ,@(let loop [(acc '())
                             (names method-names)]
                    (if (null? names)
                        acc
                        (loop
                         (cons `[(,(car names))
                                 (apply ,(car names) (drop msg 1))]
                               acc)
                         (cdr names))))
                (else (apply super msg)))))
      ,@(cdddr constructor)
      (display self)
      (newline))))

(define (setup-methods methods)
  (let [(method-names (map caadr methods))
        (method-args (map cdadr methods))
        (method-bodies (map cddr methods))]
    (let loop [(acc '())
               (names method-names)
               (args method-args)
               (bodies method-bodies)]
      (if (null? names)
          acc
          (loop (cons (setup-method (car names) (car args) (car bodies)) acc)
                (cdr names) (cdr args) (cdr bodies))))))

(define (setup-method name args body)
  `(define ,name
     (lambda ,args
       ,@body)))

(define (super-args constructor)
  (cdaddr constructor))

(define-macro (define-class name superclass static private constructor . methods)
  `(define ,name
     (let ,static
       (lambda ,(constructor-arguments constructor)
         (let ((super (apply ,superclass (list ,@(super-args constructor))))
               (self (Object)) ,@private)
           ,@(setup-constructor constructor methods)
           ,@(setup-methods methods)
           self)))))

(define (Object)
  (lambda msg (error "Object accepts no messages" msg)))

#| Examples
(define-class <Name> <Superclass>
  (<static variables>)
  (<class variables>)

  (define-constructor (<constructor parameter>)
    (super <arguments to superclass>)
    (self '<method name> <method argument>)
    <constructor body>)

  (define-method (<method name> <method parameter>)
    <method-body>))

(obj '<method name> <arguments>)

(define-class Counter Object
  ()
  ((count 0))

  (define-constructor () (super))

  (define-method (increment)
    (set! count (+ count 1)))

  (define-method (get-count)
    count))


(define-class Person Object
  ()
  ((name ""))

  (define-constructor (actual-name)
    (super)
    (set! name actual-name))

  (define-method (set-name! n)
    (set! name n))

  (define-method (species)
    (display "humann")
    (newline))

  (define-method (get-name)
    name)

  (define-method (introduce)
    (display "Hi! I'm a person. ")
    (display "My name is ")
    (display name)
    (newline)))

(define-class Student Person
  ()
  ((id 0))

  (define-constructor (actual-name identity)
    (super actual-name)
    (self 'set-name! actual-name)
    (set! id identity))

  (define-method (introduce)
    (display "Hi I'm a student. ")
    (display "My name is ")
    (display (self 'get-name))
    (display " and my id is ")
    (display id)
    (newline)))
|#
