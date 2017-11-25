#lang slideshow
(require slideshow/code)
(define-syntax-rule (todo expr)
  (colorize (t expr) "red"))

(slide (titlet "Introduction to Racket")
       (blank)
       (t "Advanced Functional Programming")
       (t "Kostis Sagonas")
       (t "November 2017")
       (t "(original set of slides by Jean-Noël Monette)"))

(current-font-size (- (current-font-size) 5))
(current-para-width (* (current-para-width) 1.3))

(slide #:title "Racket"
       ;;(todo "Maybe not start with that")
       (item "a programming language — a dialect of Lisp and a descendant of Scheme")
       (item "a family of programming languages — variants of Racket, and more")
       (item "a set of tools — for using a family of programming languages"))

(slide #:title "Getting Started"
       ;; (todo "Make links working")
       (para "Installation ")
       (item "download from" (hyperlinkize (t "http://www.racket-lang.org/download")))
       (item "run the self-extracting archive")
       (item "type" (tt"racket") " (REPL) or" (tt"drracket") " (IDE)")
       (blank)
       'next
       (para "Documentation at" (hyperlinkize (t "http://docs.racket-lang.org")) " which contains:")
       (item "tutorials")
       (item "comprehensive guide")
       (item "reference manual")
       (item "... and a lot more ..."))

(define-syntax code-reduce
  (syntax-rules ()
    ((_ exp) 
     (let ((t exp))
       (hb-append (code exp) (tt " => ") (code (unsyntax t)))))))     

(slide #:title "A First Example"
       (code \#lang racket
             code:blank
             (define (fact x)
               (cond [(> x 0) (* x (fact (sub1 x)))]
                     [else 1]))
             code:blank
             (fact 42)))

(slide #:title "Syntax"
       (item "The syntax is uniform and made of s-expressions")
       (item "An s-expression is an atom or a sequence of atoms separated by spaces and enclosed in parentheses")
       (item "Square brackets" (tt "[]") "and braces" (tt "{}") " can be used instead of parentheses (as long as they match per type)")
       (item "There are a few syntactic shortcuts such as e.g." (tt "' , #")))

(slide #:title "Choosing your language"
       (para "Always start your files with" (tt "#lang racket") " to define the language.")
       (para "We will mainly use" (tt "racket") " as a language but others do exist.")
       (para "Examples:" (tt "typed/racket")", "(tt "slideshow")", "(tt "scribble")", ..."))

(slide #:title "Racket Basics"
       (para "Strict evaluation: ")
       (item "arguments to a procedure are evaluated before the procedure")
       (para "Dynamically typed: ")
       (item (code (fact "some string")) " is a runtime error, not a compilation error")
       'next
       (t "But")
       'next
       (para "Macros can emulate laziness")
       (para "Contracts can help catch \"type errors\""))

(slide #:title "Comments"
       (para (tt ";") "starts a comment to the end of the line")
       (para (tt ";;") "is usually used to mark more important comments")
       (para (tt "#;") "comments the following s-expression"))

(slide #:title "Procedure Calls"
       (para "Appear between parentheses")
       (item "the first expression must evaluate to the procedure")
       (item "the remaining ones are the arguments")
       (blank)
       (code (+ 1 2 3 4) 
             (string? "Hello")
             (equal? 42 "bar")))

(slide #:title "Definitions"
       (code (define x 5)
             (define (inc x) (+ x 1)) (code:comment "predefined as add1")
             (define 3*2 (* 3 2)))
       (blank)
       (para "Identifiers can be composed of any characters but" (tt "()[]{}\",'`;#|\\"))
       (para "Identifiers usually start with a lower case letter")
       (para "Compound names are usually separated with "(tt"-")" , e.g." (tt"sum-two-numbers")))

(slide #:title "Numbers"
       ;;(todo "Lookup the numbers in the documentation")
       (item "Arbitrary large integers and (exact) rationals:" (code (/ 1 3)))
       (item "Floating point numbers:" (code (+ 3.14 -inf.0 +nan.0)))
       (item "Complex numbers:" (code 42+1/2i))
       (item (htl-append (t "Test procedures: ")
			 (code number? real? rational? integer?
			       inexact? exact?))))

(slide #:title "Booleans"
       (para "Two boolean literals: " (code #t) " and " (code #f))
       (para "Everything not" (code #f) "is considered as true in conditions")
       (para (code (boolean? x)) "tells whether" (code x) "is a boolean value")
       (para (code (and)) "and" (code (or)) "take any number of arguments (including zero) and short-circuit")
       (item "For instance," (code (or 42 #f)) "returns" (code 42)))

(slide #:title "Characters and Strings"
       (para "Characters are Unicode scalar values:")
       (code #\A)
       (para "Converting to/from integers with" (tt "char->integer") "and" (tt "integer->char"))
       (para "Strings are sequences of characters (in between double quotes):")
       (code "Hello, World!")
       (code (string-length (string-append "Hello" ", " "World!"))))

(slide #:title "Comparing Objects"
       (para "There are (at least) four comparison procedures in Racket")
       (item (code =) "compares numbers numerically:")
       (code-reduce (= 1 1.0))
       (code-reduce (= 0.0 -0.0))
       (code-reduce (= 1/10 0.1))
       'next
       (item (code eq?) "compares objects by reference:")
       (code-reduce (eq? (cons 1 2) (cons 1 2)))
       (code-reduce (let ([x (cons 1 2)]) (eq? x x)))
       (para "   This is fast but not reliable in all cases"))

(slide #:title "Comparing Objects (2)"
       (item (code eqv?) "is like" (code eq?) "except for numbers and characters:")
       (code-reduce (eq? (expt 2 100) (expt 2 100)))
       (code-reduce (eqv? (expt 2 100) (expt 2 100)))
       'next
       (item (code equal?) "is like" (code eqv?) "except for strings and decomposable structures (lists, hash-table, structures):")
       (code-reduce (eqv? "banana" (string-append "ban" "ana")))
       (code-reduce (equal? "banana" (string-append "ban" "ana")))
       (code-reduce (equal? (list 1 2) (cons 1 (cons 2 '()))))
       'next
       (blank)
       (para "Suggestion: prefer the use of" (code equal?) "as it is more reliable, and" (code =) "for (exact) numeric comparisons."))

(slide #:title "Conditionals"
       (code (if (> 1 0) "Good" 'nogood))
       (code (cond [(not (number? x)) "NaN"]
                   [(> x 0) "Pos"]
                   [(< x 0) "Neg"]
                   [else "Zero"]))
       (para "If no condition evaluates to true and there is no" (code else) "clause, the result is" (code (void))))

(slide #:title "Printing"
       (para "There are (at least) three ways to output data to the console:")
       (item (code display) "removes all quotation marks and string delimiters")
       (item (code print) "does not remove any quotation marks or string delimiters")
       (item (code write) "removes the outermost quotation mark if any")
       (para "In addition," (code (newline)) "prints a newline.")
       (code (displayln '(a "azer" 3))
             (print '(a "azer" 3))
             (newline)
             (write '(a "azer" 3))))

(slide #:title "Anonymous Procedures"
       (para (code (lambda (x) (+ x 1))) "defines an anonymous procedure")
       (blank)
       (code (define inc (lambda (x) (+ x 1))) 
             (inc 41)
             ((lambda (x) (+ x 1)) 41)))

(slide #:title "Procedure Body"
       (item "A procedure body is composed of any number of (local) definitions followed by any number of expressions")
       (item "The return value of the procedure is the value of the last expression")
       (item "Internal defines can be mutually recursive:")
       (code (define (sum a b)
	       (define (suma c) (+ a c))
	       (suma b)))
       (para "   Here" (code sum) "is defined at the top-level, while" (code suma) "is a local definition. "))

(slide #:title "Local Declarations"
       (para (tt "let") " declares local variables. It evaluates all the expressions before binding them.")
       (code (let ([x y] [y x])
               (cons x y)))
       'next
       (para "In a "(tt "let*") ", the first bindings are available to the next ones.")
       (code (let* ([x y] [y x])
               (cons x y)))
       'next
       (para "In " (tt "letrec") ", all bindings are available to each other (mainly for mutually recursive local procedures).")
       (code (letrec ([x y] [y x])
               (cons x y))))

(slide #:title "Local Declaration of Procedures"
       (code (let loop () (loop)))
       (para "This creates a procedure called" (code loop) "and executes it.")
       (para "This particular example is probably not very interesting...")
       'next
       (para "Below," (code sum-help) "is a procedure of two (optional) arguments")
       (code (define (sum x)  
               (let sum-help ([x x] [res 0])
                 (cond [(= x 0) res]
                       [else (sum-help (sub1 x) (+ res x))])))))

(slide #:title "Lists"
       (code (list 1 2 3 4)
             (define x (list 1 2 3 4))
             (car x) (first x) 
             (cdr x) (rest x) 
             null empty 
             (cons 0 x) 
             (cons? x) (pair? x)
             (null? x) (empty? x) 
             (length (list 9 8 7))
             (map add1 (list 1 2 3 4))
             (andmap string? (list "a" "b" 0))
             (filter positive? (list -1 0 1 2 -5 4))
             (foldl + 0 (list 1 2 3)))
       )

(slide #:title "Cons revisited"
       (para (code (cons 1 2)) "is valid code but it does not produce a proper list.")
       (para (code (list? x)) "tells if it is a proper list (in constant time).")
       (para "This is a difference between strongly typed code (such as SML) and Racket."))

(slide #:title "Dots and Infix Notation"
       (para "A fake list is displayed like that:")
       (code '(1 2 . 3))
       (para "One can also use it when entering a list:")
       (para (tt "'(1 2 . (3 4))") "is equivalent to the list " (code '(1 2 3 4)))
       'next
       (para "One can also use two dots around an element of the s-expr to make it the first one.")
       (code (code (4 . + . 5)) " is transformed into " (code (+ 4 5)))
       (para "This can be useful if you are not comfortable with the prefix notation."))

(slide #:title "Quotation and Symbols"
       (para (code (list '+ 2 3 4)) "produces a list" (code '(+ 2 3 4)) " that looks like a procedure application but is not evaluated and preceded by " (tt "'"))
       (para "The s-expression is " (it "quoted") " and considered as data.")
       (para (code quote) " quotes its argument without evaluating it.")
       (para (tt "(quote (map + 0 \"cool\"))") "is simply a list of four elements.")
       'next
       (para (tt "(quote map)") " creates a" (it "symbol") (code 'map) " that has nothing to do with the identifier" (code map) " (except the name).")
       (para "One can directly write" (tt "'") "instead of" (code quote) ".")
       (para (code quote) "has no effect on literals (numbers, strings)")
       (para "Symbols can be also created with" (code (string->symbol "aer")) "or" (code (gensym)))
       )
       
(slide #:title "Quasiquoting and Unquoting"
       (para "Quasiquoting is like quoting but it can be escaped to evaluate part of the expression:")
       (para (vr-append (tt "          (quasiquote (1 2 (unquote (+ 1 2)) ")
                        (tt "(unquote (- 5 1)))")))
       (para "Or equivalently:")
       (code-reduce `(1 2 ,(+ 1 2) ,(- 5 1)))
       'next
       (para (tt ",@") " or " (code unquote-splicing) "also decompose a list:")
       (code-reduce `(1 2 ,@(map add1 '(2 3))))
       (code-reduce `(1 2 ,(map add1 '(2 3)))))

(slide #:title "Eval"
       (para "(Quasi-)quoted s-expressions can be evaluated using " (code eval))
       (blank)
       (code (define sum ''(+ 1 2 3 4))
             (displayln sum)
             (displayln (eval sum))
             (displayln (eval (eval sum)))))

(slide #:title "Apply"
       (para (code apply) "applies a procedure to a list of arguments:")
       (code (apply + '(1 2 3)))
       (para "With more than one argument, the first ones are put in front of the list:")
       (code (apply + 1 '(2 3)))
       (code (apply append '(1 2) '((3 4)))))

(slide #:title "Procedure Arguments"
       (para "Procedures can have a variable number of arguments:")
       (code (define (proc1 . all) (apply + (length all) all))
               (proc1 12 13 14)
	       (proc1)
	       (proc1 41)
             (define (proc2 x . rest) (* x (length rest)))   
               (proc2 7 1 2 3 4 5 6)
	       (proc2 42 0)
	       (proc2))
       'next
       (para "There can also be optional and keywords arguments: ")
       (code (define (proc3 x [y 2]) (+ x y))   (proc3 40)
             (define (proc4 x #:y y) (- x y))   (proc4 #:y 2 44)
             (define (proc5 x #:y [y 7]) (* x y))   (proc5 6)
             (define (proc6 x #:y y . rest) ...)))

(slide #:title "Curried and Higher-Order Procedures"
       (para "Short way to define curried procedures:")
       (code (define ((add x) y) (+ x y))
             (define add38 (add 38))
             (add38 4)
             ((add 11) 31))
       'next
       (para "A simple composition of procedures:")
       (code (define ((comp f g) . x)
               (f (apply g x)))
             (define add2 (comp add1 add1))
             (add2 40)))

(slide #:title "Multiple Values"
       (para "A procedure can return several values at the same time with" (code values)":")
       (code (values 1 2 3))
       (para "To bind those values to identifiers, one can use " (code define-values) ", or " 
             (code let-values) ", or one of the other variants (e.g. "(code let-values)"):")
       (code (define-values (x y z) (values 1 2 3))
             (define-values (five) (add1 4))))

(slide #:title "Simple Matching: case"
       (para (code case) " matches a given value against fixed values (with " (code equals?) ")")
       (code (case v
               [(0) 'zero]
               [(1) 'one]
               [(2) 'two]
               [(3 4 5) 'many]
               [else 'too-many]))
       (para "If no branch matches and there is no" (code else) "clause, the result is " (tt "#<void>")"."))

(slide #:title "More Matching: match"
       (para (code match) "matches a given value against patterns.")
       (para "Patterns can be very complex (using e.g." (code and)"," (code or)"," (code not)"," (code regexp)", ...):")
       (scale (code (match x
                      ['() "Empty"]
                      [(cons _ '()) "A list that contains one element"]
                      [(cons a a) "A pair of identical elements"]
                      [(or (list y ...) (hash-table y ...)) 
                       "A list or a hash table"]
                      [(? string?) "A string"]
                      [else "Something else"])) 0.9)
       (para "If no branch matches, an" (code exn:misc:match?) "exception is raised."))

(slide #:title "Assignment"
       (para "The value bound to an identifier can be modified using" (code set!) )
       (code (define next-number!
               (let ([n 0])
                 (lambda ()
                   (set! n (add1 n))
                   n)))
             code:blank
             (next-number!)
             (next-number!))
       (para "Use with care!"))

(slide #:title "Guarded Operations"
       ;;(todo "Maybe not so interesting")
       (code (when with-print (print x))
             (unless finished (set! x y)))
       (para "Mainly useful to enclose side-effect only code.")
       (blank)
       'next 
       (blank)
       (para "Quiz: What is the return value of the following code?")
       (code (when #f #t))
       'next
       (para "          Also produced by the procedure" (code (void)) "."))

(slide #:title "Parameters"
       (para "Parameters are variables that can be dynamically bound:")
       (code (define color (make-parameter "green"))
             (define (best-color) (display (color)))
             (best-color)
             (parameterize ([color "red"])
               (best-color))
             (best-color))
       (para "This is preferable to" (code set!) " for several reasons (tail calls, threads, exceptions).")
       (para "There exist parameters for instance to define the output stream, the level of details when reporting an error, etc."))

(slide #:title "Vectors"
       (item "Fixed length arrays with constant-time access to the elements")
       (item "Created as a list but with a " (tt "#") "instead of the quotation mark or with the procedure" (code vector))
       (code (vector "a" 1 #f))
       (item (code (vector-ref a-vect num)) "accesses the" (code num)"-th element of" (code a-vect) "(indices start from zero)")
       (item "Vector elements can be modified with" (code vector-set!))
       (code (vector-set! a-vect num new-val)))

(slide #:title "Hash Tables"
       (para "Immutable hash tables:")
       (code (define ht (hash "a" 3 "b" 'three))
             (define ht2 (hash-set ht "c" "three"))
             (hash-ref ht2 "c")
             (hash-has-key? ht "c"))
       (blank)
       'next
       (para "Mutable hash tables:")
       (code (define ht (make-hash '(("A" "Apple") 
                                     ("B" "Banana"))))
             (hash-set! ht "A" "Ananas")
             (hash-ref ht "A")
             )
       ;;Iteration over hash tables.
       )

(slide #:title "New Datatypes"
       (para (code (struct point (x y))) "produces a new data structure that can be used as follows:")
       (code (point 1 2)
             (point? (point 1 2))
             (point-x (point 1 2)))
       'next
       (blank)
       (para "We can also create data structures whose internal structure can be accessed (e.g. recursively by" (code equals?)"), and its fields can be modified:")
       (code (struct point (x y) #:transparent #:mutable)))

(slide #:title "Exceptions"
       (para "Exceptions are raised upon runtime errors.")
       (para "To catch exceptions use an exception handler:")
       (code (with-handlers ([exn:fail:contract:divide-by-zero?
                              (lambda (exn) +inf.0)])
               (/ 1 0)))
       (para "The first argument is a list of pairs, whose first element is a test to check the type of exception and its second is what to do with it.")
       (para "The check" (code exn:fail?) "catches all exceptions.")
       (para (code (error "string")) "creates and raises a generic exception.")
       (para (code (raise 42)) "raises anything as an exception."))

(slide #:title "Threads"
       (para (code thread) "runs the given procedure in a separate thread and returns the thread identifier.")
       (code (define t (thread (lambda () 
                                 (let loop ()
                                   (display "In thread")
                                   (sleep 1)
                                   (loop)))))
             (sleep 142)
             (kill-thread t))
       (para "Threads are lightweight and run inside the same physical process."))

(slide #:title "Threads and Channels"
       (para "Threads can collaborate (among others) through message passing with" (code thread-send) "and" (code thread-receive)":")
       (code (define t0 (current-thread))
             (define t1 
               (thread (lambda () 
                         (define v (thread-receive))
                         (thread-send t0 (add1 v)))))
             (thread-send t1 41)
             (display (thread-receive))))

(slide #:title "Comprehensions"
       (para "Racket provides many looping constructs:")
       (code (for ([i '(1 2 3 4 5)])
               (display i))
             (for/list ([i '(1 2 3 4 5)])
               (modulo i 3))
             (for/and ([i '(1 2 3 4 5)])
               (> 0))
             (for/fold ([sum 0])
               ([i '(1 2 3 4 5)])
               (+ sum i))))

(slide #:title "Parallel and Nested Comprehensions"
       (para (code for) "and variations iterate over several sequences in parallel:")
       (code (for ([i '(1 2 3 4)]
                   [j '(1 2 3)])
               (display (list i j))))
       'next
       (para (code for*) "and variations act as nested" (code for)"'s:")
       (code (for* ([i '(1 2 3 4)]
                    [j '(1 2 3)])
               (display (list i j)))))

(slide #:title "Iterable Sequences"
       (para (code for) "and variations can iterate over different kinds of sequences, not only lists:")
       (code (for ([(k v) (hash 1 "a" 2 "b" 3 "c")]
                   [i 5]) (code:comment "range 0 to 4")
               (display (list i k v)))
             (for ([i "abc"]
                   [j (in-naturals)])
               (display (cons i j)))))

(slide #:title "Performance of Sequences"
       (para "To make the comprehension fast, one should \"declare\" the type of each sequence.")
       (code (for ([i (in-range 10)]
                   [j (in-list '(1 2 3 4 5 6))]
                   [k (in-string "qwerty")])
               (display (list i j k)))))

(slide #:title "There is Much More in Racket"
       (item "Classes and Objects")
       (item "Units and Signatures")
       (item "Input/Output")
       (item "RackUnit")
       (item "Graphical, Network, Web, DB, ... Libraries")
       (item "Other Languages (Typed Racket, Scribble, ...)"))
             
(slide #:title "Wrap-up"
       (item "Everything you can expect from a modern functional language")
       (item "Minimal syntax — although a bit of an acquired taste")
       (item "Code = Data")
       (blank)
       'next
       (para "Next Lectures")
       (item "Macros")
       (item "Modules and Contracts")
       (item "Making your own Language"))

(slide #:title "Voluntary Warm-up Exercises"
       (item "Redefine" (code map) "and" (code length) "as recursive procedures.")
       (item "Define a procedure" (code (primes n)) "that returns a list of the "(code n)" first prime numbers.")
       (item "Define a procedure" (code (perms xs)) "that returns all permutations of the list" (code xs) "."))

