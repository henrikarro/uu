#lang slideshow
(require slideshow/code)

(slide (titlet "Racket: Modules, Contracts, Languages")
       (blank)
       (t "Advanced Functional Programming")
       (t "Kostis Sagonas")
       (t "November 2017")
       (t "(original set of slides by Jean-Noël Monette)"))

(current-font-size (- (current-font-size) 5))
(current-para-width (* (current-para-width) 1.3))

(slide #:title "Content"
       (para "Modules are the way to structure larger programs in smaller pieces.")
       (para "Modules can import and export bindings.")
       (para "Contracts define conditions on the exported bindings.")
       (para "Modules can be used to define new languages."))
         
(require (prefix-in modules: "slides-modules.rkt"))
(modules:slides)

(slide #:title "Contracts"
       (para "Contracts are used to define the pre- and post-conditions on provided procedures.")
       (para "They are defined as part of the" (code provide) "instruction.")
       (para "The contract system ensures that contracts are always respected."))

(slide #:title "A first contract"
       (code (provide (contract-out [fact (-> natural-number/c 
                                              natural-number/c)])))
       (para "The contract stipulates that" (code fact) "is a function taking a natural and returning a natural.")
       'next
       (para (code natural-number/c) " is provided by the system but we can write our own test:")
       (code (define (nat? x) 
               (and (number? x) (integer? x) (exact? x) (>= x 0)))
             (provide (contract-out [fact (-> nat? nat?)])))
       (para "A basic contract just needs to be a procedure taking one argument. Its return value is interpreted as a \"boolean\"."))

(slide #:title "Contract language (examples)"
       (para (code (and/c number? integer?)) " ensures the two conditions.")
       (para (code (or/c number? string?)) " ensures one of the conditions.")
       (para (code any) " accepts any argument(s).")
       (para (code any/c) " accepts one argument of any type.")
       (para (code (listof string?)) " accepts a list of strings.")
       (para "Literals (e.g., symbols, numbers) are evaluated as a contract accepting only themselves. "
             "Example: " (code (or/c 'bold 'italic #f)))
       'next
       (para "Side Question: how would you implement e.g. " (code or/c) "?")
       'next
       (code (define ((or/c . tests) x)
               (ormap (lambda (test) (cond [(procedure? test) (test x)]
                                           [else (equal? test x)]))
                      tests))))

(slide #:title "Functions with optional and rest arguments"
       (para "To have a variable number of arguments, use the" (code ->*) "combinator that takes three arguments (mandatory, optional, return).")
       (code (define (silly x y [z 1]) (* (+ x y) z))
             code:blank
             (provide (contract-out [silly (->* (integer? integer?) 
                                                (integer?) 
                                                integer?)])))
       'next
       (para "One can also put a contract on the rest.")
       (code (provide (contract-out [max (->* ()
                                              ()
                                              #:rest (listof real?)
                                              real?)]))))

(slide #:title "Dependencies"
       (code (struct counter (cnt))
             (define (count) (counter 0))
             (define (inc cnt) (counter (add1 (counter-cnt cnt))))
             (define (val cnt) (counter-cnt cnt)))
       (para "How to express that the value stored in the result of" (code inc) " is the value of the argument plus 1?")
       'next
       (para "With what we have seen so far, it is not possible.")
       (para "We need to use yet another combinator:" (code ->i)))

(slide #:title "Specifying dependencies"
       (para (code ->i) "looks like" (code ->*) "but all arguments are given name so that they can be reused.")
       (code (define (minus a b) (- a b))
             (provide 
              (contract-out 
               [minus (->i ([a natural-number/c]
                            [b (a) (and/c natural-number/c (<=/c a))])
                           [result (a) (and/c natural-number/c 
                                                (<=/c a))])]))))

(slide #:title "Example: counter"
       (scale (code (module counter racket
                      (struct counter (cnt))
                      (define (count) (counter 0))
                      (define (inc cnt) (counter (add1 (counter-cnt cnt))))
                      (define (val cnt) (counter-cnt cnt))
                      code:blank
                      (provide 
                       (contract-out 
                        [count (-> (and/c counter? (lambda (res) (= 0 (val res)))))]
                        [val (-> counter? natural-number/c)]
                        [inc (->i ([cnt counter?])
                                  [res (cnt) 
                                       (and/c counter? 
                                              (lambda (res) 
                                                (= (val res) (add1 (val cnt)))))])])))
                    code:blank
                    (require 'counter)
                    (define x (count))
                    (val x)
                    (define y (inc (inc x)))
                    (val y)) 0.8))

(slide #:title "Example: stack"
       (code 
       (provide make-stack (code:comment "empty stack")
                list->stack (code:comment "already filled stack")
                stack->list (code:comment "The content of the stack in a list")
                empty-stack? (code:comment "Is the stack empty?")
                size (code:comment "The number of elements in the stack")
                top (code:comment "The first element of the stack")
                pop (code:comment "The stack without its top element")
                push (code:comment "The stack with a new element on top")
                stack?) (code:comment "Is this object a stack?")
                code:blank))

(slide #:title "Stack: implementation"
       (code (struct stack (list))
             code:blank
             (define (make-stack) (stack '()))
             (define list->stack stack)
             (define stack->list stack-list)
             (define empty-stack? (compose null? stack-list))
             (define size (compose length stack-list))
             (define top  (compose car stack-list))
             (define pop (compose stack cdr stack-list))
             (define (push s el) (stack (cons el (stack-list s))))
             (code:comment "stack? is provided by the struct."))
       (para (code top) "and" (code pop) "need a non-empty stack. This is not handled here but in the contracts."))

(slide #:title "Stack: simple contracts"
       (para "First, we put contracts on types and pre-conditions.")
       'next
       (code (provide 
              (contract-out 
               [make-stack (-> stack?)]
               [list->stack (-> (listof any/c) stack?)]
               [stack->list (-> stack? (listof any/c))]
               [empty-stack? (-> stack? boolean?)]
               [size (-> stack? natural-number/c)]
               [top (-> (and/c stack? (not/c empty-stack?)) any/c)]
               [pop (-> (and/c stack? (not/c empty-stack?)) stack?)]
               [push (-> stack? any/c stack?)]
               [stack? (-> any/c boolean?)]))))

(slide #:title "Stack: what is a stack?"
       (para "The previous contracts would still be valid if our implementation was actually a queue.")
       (para "What is the main characteristic of a stack?")
       'next
       (para "LIFO: Last-In-First-Out")
       (para "The elements are popped in the opposite order than they were pushed.")
       (para "Condition on" (code push) ": the pushed element is now on" (code top) "."))

(slide #:title "Contract on push"
       (code (...
              [push (->i ([st stack?]
                          [elem any/c])
                         [res (elem) (and/c stack? 
                                            (lambda (res) 
                                              (equal? (top res) elem)))])]
              ...))
       'next 
       (para "Exercise: What about the property that the rest of the stack is preserved?"))


(slide #:title "Contracts for more complex cases"
       (para "Many more complex contracts are available.")
       (para "See the guide and reference.")
       'next
       (para "Contracts are checked at runtime. Hence they incur an overhead.")
       (para "Module boundary is a good place to check contracts.")
       (para "One should avoid writing contracts that are too costly to check."))


(slide #:title "New Languages"
       (para "Macros can only extend the language, and do it inside the syntactic convention of the language.")
       (para "If one wants to create a new language, it may be necessary to restrict or alter the language, or to change the syntax.")
       (para "We will see how to do that, but not the syntax part."))

(slide #:title "New Languages (2)"
       (para "The \"language\" of a module can be defined arbitrarily.")
       (para "In" (code (module name language body ...))", the argument" (code language) 
             "can be any module.")
       (para "The bindings provided by the" (code language) "module define what is available to the new module.")
       (para "One can define what is available in a language in the" (code provide) " instruction."))

(slide #:title "A first language"
       (scale (code (module racketf racket (code:comment "Like racket but \"lambda\" is \"function\"")
               (provide (except-out (all-from-out racket) lambda)
                        (rename-out [lambda function])))
             code:blank
             (module mymodule (submod ".." racketf) (code:comment "Use the new language")
               (define sqr (function (x) (* x x)))
               (code:comment "(define sqr (lambda (x) (* x x))) ; would be an error")
               (sqr 5))
             code:blank
             (require 'mymodule)) 0.95))

(slide #:title "Minimal export"
       (code (module minimal racket
               (provide #%app (code:comment "Implicit form for procedure application")
                        #%module-begin (code:comment "Implicit for module declaration")
                        #%datum (code:comment "Implicit for literals and data")
                        #%top (code:comment "Implicit for unbound identifiers")
                        lambda)) (code:comment "Just because we want to do something")
             code:blank
             (module test  (submod ".." minimal)
               (code:comment "ok")
               ((lambda (x) x) 10) 
               (code:comment "not ok")
               ((lambda (x) (+ x 1)) 10))))

(slide #:title "Redefining implicit forms"
'alts (list (list
       (code \#lang racket
             (module verbose racket 
               (provide (except-out (all-from-out racket)
                                    #%module-begin
                                    #%app
                                    #%top
                                    #%datum)
                        (rename-out [module-begin #%module-begin]
                                    [app #%app]
                                    [top #%top]
                                    [datum #%datum]))
               code:blank
               (define-syntax-rule (module-begin expr ...)
                 (#%module-begin
                  (displayln "Entering Module Verbose")
                  expr ...
                  (displayln "Leaving Module Verbose")))
               code:blank
               [...])))
            (list
       (code (define-syntax-rule (app f arg ...)
               (begin (display "Applying: ")
                      (displayln (quote (f arg ...)))
                      (let ([res (#%app f arg ...)])
                        (display "  res: ")
                        (displayln res)
                        res)))
             code:blank
             (define-syntax-rule (top . arg)
               (begin (display "Not found ")
                      (displayln (quote arg))
                      'arg))
             code:blank
             (define-syntax-rule (datum . arg)
               (begin (display "Value: ")
                      (displayln (quote arg))
                      (#%datum . arg)))))
            (list
      (code (module client (submod ".." verbose)
              (define x 5)
              (+ x 10 x)
              (display y)
              )
            code:blank
            (require 'client))
      (para "Result:")
      (code Entering Module Verbose
            Value: 5
            Applying: (+ x 10 x)
            Value: 10
            res: 20
            20
            Applying: (display y)
            Not found y
            y  res: \#<void>
            Leaving Module Verbose))))

(slide #:title "A new #lang"
       (code \#lang s-exp "fname.rkt"
             ...)
       (para " is equivalent to ")
       (code (module name "fname.rkt"
               ...))
       'next
       (para "To change the syntax, or to get rid of the \"s-exp\", see the guide."))

(slide #:title "Example: Half-Life"
       (para "Everytime an identifier is used, its value is divided by two.")
       (code \#lang s-exp "Language-half-life.rkt"
             code:blank
             (define x 10)
             (define y 10)
             (+ (* x y) (* x y))
             (define z (* 50 y (- x x)))
             (+ (if (= z 100) z 100) y)
             z)
       'next
       (para "Produces...")
       (code 125
             51
             25))

(slide #:title "Half-Life: Implementation"
'alts (list (list
             (code \#lang racket
                   code:blank
                   (provide + - / * = > < >= <= if
                            #%app #%datum
                            #%module-begin
                            #%top-interaction
                            (rename-out [my-define define]
                                        [lookup #%top]))
                   code:blank
                   (define env (make-hash))
                   code:blank
                   (define-syntax-rule (my-define id expr)
                     (let ([val expr])
                       (hash-set! env 'id val)
                       (void)))))
            (list 
             (code (define-syntax-rule (lookup . id)
                     (if (hash-has-key? env 'id)
                         (let* ([val (hash-ref env 'id)]
                                [new-val (floor (/ val 2))])
                           (if (not (= new-val 0)) 
                               (hash-set! env 'id new-val)
                               (hash-remove! env 'id))
                           val)
                         (error "This id does not exist (anymore)")))))))

(slide #:title "Summary"
       (item "Modules are used to structure large programs in smaller pieces.")
       (item "It is possible to define contracts on the provided procedures.")
       (item "Creating a new language amounts to using macros and the module system."))

(slide #:title "General summary"
       (para "Racket is a modern functional programming language.")
       (para (code {if {you get sick of '()} {use {'[] or '{}}}}) ":-)")
       (para "It is more than one language:")
       (item "several existing languages")
       (item "the infrastructure to create your own."))

       
#;(slide #:title "Raco"
       (para (tt "raco") "is the tool to compile and manage your code.")
       (para (tt "raco make fname") "compiles files.")
       (para (tt "raco link dirname") "to create a collection.")
       (para (tt "raco setup colname") "to compile a collection.")
       (para (tt "raco test fname") "runs the tests defined in submodules."))

;(slide #:title "rackunit"
;       (para "defines useful functions for unit testing."))


                     
