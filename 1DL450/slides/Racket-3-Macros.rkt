#lang slideshow

(require slideshow/code)

(slide (titlet "Racket: Macros")
       (blank)
       (t "Advanced Functional Programming")
       (t "Kostis Sagonas")
       (t "November 2017")
       (t "(original set of slides by Jean-Noël Monette)"))

(current-font-size (- (current-font-size) 5))
(current-para-width (* (current-para-width) 1.3))

(slide #:title "Content"
       (item "Macros")
       (subitem "Pattern-based macros")
       (subitem "Hygiene")
       (subitem "Syntax objects and general macros")
       (subitem "Examples"))

(slide #:title "Macros"
       (para "(According to the Racket Guide...) A macro is a new syntactic form that can be expanded into existing forms through a transformer.")
       (para "In general, it is a way to extend the language with new constructs.")
       (para "Let's start with a simple example. Say we do not have the" (code or) "construct and we want to implement it in terms of" (code if)"."))

(slide #:title "Implementing or: First try"
       (code (define (or x y)
               (if x x y)))
       'next
       (para "What are the problems?")
       'next
       (code (or 3 (/ 2 0)))
       'next
       (item (code y) " is always evaluated.")
       'next 
       (para "A function does not work. We need something that would transform the" (code or) "into an" (code if) "before run time."))

(slide #:title "Using define-syntax-rule"
       (para (code define-syntax-rule) "defines a macro.")
       (code (define-syntax-rule (or x y)
                 (if x x y)))
       (para (code (or 3 (/ 2 0))) "is effectively transformed into" (code (if 3 3 (/ 2 0)))".")
       'next
       (para "The problem is solved. But we introduced another one:")
       (code (or (begin (display 3) 3) (/ 2 0)))
       (para (code x) "is evaluated twice."))

(slide #:title "Introducing let"
       (code (define-syntax-rule (or x y)
               (let ([z x])
                 (if z z y)))
             code:blank
             (or 3 (/ 2 0))
             (or (begin (display 3) 3) (/ 2 0)))
       'next
       (para "Seems like we are good."))

(slide #:title "Hygiene"
       (para "Consider another example")
       (code (define-syntax-rule (swap! x y)
               (let ([tmp x])
                 (set! x y)
                 (set! y tmp))))
       'next
       (para "What if we pass a variable named" (code tmp) "(or even" (code set!) "or" (code let)") to" (code swap!)"?")
       'next
       (para "The naive rewriting would produce an erroneous code.")
       (para "Hopefully, the racket macro system is \"hygienic\"; that is, it makes sure to avoid clashes between identifiers and it uses lexical scoping."))

(slide #:title "More complicated macros"
       (code (define-syntax or
               (syntax-rules ()
                 [(or) #f]
                 [(or x) x]
                 [(or x y) (let ([z x])
                             (if z z y))]
                 [(or x y ...) (or x (or y ...))])))
       (item (code syntax-rules) " defines several rules by pattern matching.")
       (item (code ...) " in the pattern matches several (zero, one or more) occurences of the last pattern ("(code y) "in this case).")
       (item (code ...) "in the template provides all the bound occurences."))

#;(slide #:title "Macro generating macro"
       (para "It is possible to generate macros through macros.")
       (para "The simplest one is probably:")
       (code (define-syntax-rule (define-macro x body)
               (define-syntax-rule x
                 body))))

(slide #:title "Using literal keywords"
       (code (define-syntax while
               (syntax-rules (do)
                 [(while cond do body ...) 
                   (let loop () 
                     (when cond 
                       body ... 
                       (loop)))]))
             code:blank
             (while (> x 0) do
                    (displayln x)
                    (set! x (- x 1))))
       (para (code do) "is part of the macro definition."))

(slide #:title "A wrong version of while"
       (code (define-syntax while
               (syntax-rules (do)
                 [(while cond do body ...)
                  (when cond 
                    body ... 
                    (while cond do body ...))])))
       (t "Why?")
       'next
       (para "The macro are expanded before runtime.")
       (para "In this case, the expansion never completes."))


(slide #:title "Examples"       
       (item "Memoization")
       (item "Rewriting"))

(slide #:title "Example: Implementing Memoization"
       (para "Memoization is to record the result of previous calls to a procedure to avoid recomputing them again.")
       (para "For instance, a (naive) fibonacci procedure takes exponential time to complete.")
       (code (define (fib x) 
               (case x 
                 [(0 1) 1]
                 [else (+ (fib (- x 1)) (fib (- x 2)))])))
       (para "Memoization would allow us to reuse already computed results.")
       (para "We will define a macro" (code define-memo) "to define a procedure with memoization.")
       (para "Note: memoization only makes sense for procedures that are really functional, i.e., without side-effects."))

(slide #:title "define-memo"
       (scale (code (define-syntax-rule (define-memo (f args ...) body ...)
                      (define f 
                        (let ([memo (make-hash)])
                          (lambda (args ...)
                            (cond [(hash-has-key? memo (list args ...)) 
                                   (hash-ref memo (list args ...))]
                                  [else 
                                   (let ([res (begin body ...)])
                                     (hash-set! memo (list args ...) res)
                                     res)])))
                        ))) 0.9)
       (para "Previous results are stored in a (mutable) hash-table.")
       (para "The keys are the arguments of the function.")
       (para "The actual body of the function is executed only if the arguments are not already in the hash-table."))

(slide #:title "define-memo, application"
       (scale (code (define-syntax-rule (define-memo (f args ...) body ...)
                      (define f 
                        (let ([memo (make-hash)])
                          (lambda (args ...)
                            (cond [(hash-has-key? memo (list args ...)) 
                                   (hash-ref memo (list args ...))]
                                  [else 
                                   (let ([res (begin body ...)])
                                     (hash-set! memo (list args ...) res)
                                     res)])))
                        ))
                    code:blank
                    (define-memo (fib/m x) 
                      (display x) (code:comment "side effect to see when we actually execute the body")
                      (case x 
                        [(0 1) 1]
                        [else (+ (fib/m (- x 1)) (fib/m (- x 2)))]))
                    code:blank
                    (fib/m 10)
                    (fib/m 10)) 0.8))

(slide #:title "Example: Simplifying expressions"
       (para "It is often useful to simplify expressions.")
       (para "We will do it here by rewriting rules.")
       (para "Consider this target piece of code")
       (code (define s (simplifier
                        [(+ ,x 0) -> ,x]
                        [(* ,x 1) -> ,x]
                        [(+ ,x ,x) -> (* ,x 2)]))
             code:blank
             (s '(+ (+ y 0) (* y 1))))
       (para (code simplifier) ", which is defined on the next two slides, returns a function of one argument: the expression to simplify."))

(slide #:title "Simplifying one expression"
       (code (define-syntax simplifier
               (syntax-rules (->)
                 [(simplifier (x -> y) ...)
                  (lambda (form) 
                    (match form
                      [`x `y]
                      ...
                      [else form]))])))
       (para "Each rule is rewritten as a clause in a" (code match) "expression.")
       (para "The whole code is enclosed in a procedure.")
       (para (code ->) "is part of the macro definition, not a variable."))

(slide #:title "Making it recursive"
       (para "The previous code only simplifies the root of the expression.")
       (para "We need to call the simplification on each sub-expression.")
       (para "We need to repeat the simplification until fix-point.")
       'next
       (code (define ((recur simplify) form)
               (let loop ([reduced-form 
                           (cond [(list? form) 
                                  (map (recur simplify) form)]
                                 [else form])])
                 (let ([final-form (simplify reduced-form)])
                   (if (equal? final-form reduced-form)
                       final-form
                       (loop final-form))))))
       (para (code simplify) "is the function produced by" (code simplifier) "."))

(slide #:title "Putting it together"
       (code (define-syntax simplifier
               (syntax-rules (->)
                 [(simplifier (x -> y) ...)
                  (recur (lambda (form) (code:comment "Added recur here")
                              .....))])) (code:comment ".....: the same as before")
             code:blank
            (define s (simplifier
                       [(+ ,x 0) -> ,x]
                       [(* ,x 1) -> ,x]
                       [(+ ,x ,x) -> (* ,x 2)]))
            code:blank
            (s '(+ (+ y 0) (* y 1)))))

(slide #:title "Going further"
       (para "Up to now, we have seen macros based on patterns, rewriten to a template.")
       (para "Macros can do more than that, by manipulating the syntax (almost) however you want.")
       (para "For this, we need to introduce syntax objects."))

(slide #:title "Syntax objects"
       (para (tt "(syntax (+ 1 2))") " returns a syntax object representing the data " (code (+ 1 2)) " with lexical information and source code location.")
       (para "Syntax objects can be manipulated by the macro system.")
       'next
       (code (define-syntax Hello
               (lambda (stx) 
                 (display stx) 
                 (datum->syntax stx (cdr (syntax->datum stx)))))
             (Hello + 1 3))
       (para "The body of" (code define-syntax) "must be a procedure taking a syntax object and returning a syntax object."))

(slide #:title "syntax-case"
       (para (code syntax-rules) " is a shortcut for \"simple\" macros: one writes directly the resulting code.")
       (para "In the general form, one can use the whole power of Racket to modify the syntax.")
       (para (code syntax-case) " allows one to match the syntax against patterns.")
       (para (tt "#'") " is a shortcut for " (code syntax))
       (para "There is also" (tt "#`") "and" (tt "#,") "for quasisyntax and unsyntax."))

(slide #:title "syntax-case: example"
       'alts
       (list (list
              (para "The swap example with syntax.")
              (code (define-syntax swap
                      (lambda (stx)
                        (syntax-case stx ()
                          [(swap a b) 
                           #'(let ([tmp a])
                               (set! a b)
                               (set! b tmp))])))))
             (list
              (para "The shortcut version to avoid the lambda.")
              (code (define-syntax (swap stx)
                      (syntax-case stx ()
                        [(swap a b) 
                         #'(let ([tmp a])
                             (set! a b)
                             (set! b tmp))])))
              'next
              (para "This is not better than" (code syntax-rules) "but wait..."))))
                        

(slide #:title "syntax-case: example (2)"
       (para "Checking (before runtime) that the arguments make sense")
       (code (define-syntax (swap stx)
               (syntax-case stx ()
                 [(swap a b) 
                  (cond [(and (identifier? #'a) (identifier? #'b)) 
                         #'(let ([tmp a])
                             (set! a b)
                             (set! b tmp))]
                        [else (raise-syntax-error 
                               #f
                               "not an identifier"
                               stx
                               (if (not (identifier? #'a))
                                   #'a
                                   #'b))])]))))

(slide #:title "More syntax manipulation"
       (para (code syntax-e) "returns the data in the syntax object.")
       (para (code syntax->datum) "recursively calls " (code syntax-e) ".")
       (para (code datum->syntax) "creates a syntax object from some data and the lexical information of another syntax object.")
       (para (code syntax-list) " is like " (code syntax-e) " but it unrolls a whole list.")
       (para (code with-syntax) " is similar to a " (code let) " statement but for syntax objects.")
       (para (code syntax?) " tells whether the given value is a syntax object.")
       (para (code identifier?) "tells if the syntax object represents an identifier (" (code syntax-e) " returns a symbol)."))
       
(slide #:title "Example: extended rewriting"
       (para "Now we want to be able to put guards on our rewriting rules.")
       (code (define s (simplifier
                        [(+ ,x 0) -> ,x]
                        [(* ,x 1) -> ,x]
                        [(+ ,x ,x) -> (* ,x 2)]
                        [(+ ,k1 ,k2) 
                         ? (and (number? k1) (number? k2)) 
                         -> ,(+ k1 k2)]))
             code:blank
             (s '(+ (+ y (+ 2 -2)) (* y 1)))))

(slide #:title "Extended rewriting with syntax"
       (code (define-syntax (simplifier stx)
               (define (clause expr)
                 (syntax-case expr (? ->)
                   [(x -> y) #'[`x `y]]
                   [(x ? z -> y) #'[`x (=> fail) 
                                       (when (not z) (fail)) `y]]))
               (syntax-case stx ()
                 [(_  expr ...)
                  (with-syntax ([(res ...) 
                                 (map clause (syntax->list 
                                              (syntax (expr ...))))])
                    #'(simplify (lambda (form) 
                                  (match form
                                    res
                                    ...
                                    [else form]))))]))))

(slide #:title "Breaking the hygiene"
       (para "Sometimes we want to create identifiers that are available outside of the macro.")
       (para "This happens when creating a struct, for instance.")
       (para (code (struct box (val))) "creates e.g. the procedures" (code box-val) "and" (code box?) ".")
       'next
       (para "There is a simple way to break hygiene:")
       (para "Use" (code (datum->syntax lex-cont name))" where "(code lex-cont) " is an identifier that is given to the macro.")
       (para "In that case, the new name inherits the same lexical scope."))

(slide #:title "Breaking the hygiene: example"
       (para "Define a for loop with" (code it) "bound to the current value.")
       (code (define-syntax (for stx)
               (syntax-case stx (do)
                 [(for (obj1 objs ... ) do body ...)
                  (with-syntax ([it (datum->syntax (syntax obj1) 'it)])
                    #'(let loop ([its (list objs ...)] 
                                 [it obj1])
                        body ...
                        (when (not (empty? its)) 
                          (loop (cdr its) (car its)))))]))
             code:blank
             (for (1 2 3 4) do (display it)) (code:comment "OK")
             (for (1 2 3 4) do (display its)) (code:comment "Does not work")
             code:blank))

(slide #:title "Breaking the hygiene: building names"
       (para "This piece of code shows how to build a name from another one:")
       (code (datum->syntax 
              (syntax orig-name) 
              (string->symbol 
               (string-append "prefix-" 
                              (symbol->string 
                               (syntax->datum (syntax orig-name)))))))
       (para "One need to go from a syntax object to a symbol to a string, manipulate the string, and then all the other way around."))
       

(slide #:title "Phases"
       (para "We can use normal procedures when defining macros.")
       (para "But macros are expanded before runtime.")
       (para "How is that possible?")
       'next
       (para "There are several phases.")
       (para "The identifiers in each phase are only available in that phase, unless imported.")
       (para "One can have even more than two phases."))



(slide #:title "Conclusion: word of caution"
       (para "Macros can be very powerful.")
       (para "As any powertool, they must be used with care.")
       (para "In particular, do not use a macro if a procedure can make the work."))


;(require (prefix-in modules: "slides-modules.rkt"))
;(modules:slides)
       

