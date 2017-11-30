# Racket lab

## 0 Background

### [DrRacket](http://docs.racket-lang.org/drracket/index.html)

#### General

DrRacket provides rudimentary features for Racket programming, you could experiment them by launch DrRacket directly. Some particularly
interesting/useful ones are highlighted here:

* `Ctrl + T` -> New tab so that you could switch between multiple files quickly
* View -> Show Line Numbers
* `Ctrl + I` for Reindenting all lines
* Racket -> Comment/Uncomment for selected lines or the current line
* `Ctrl + Backslash` for inserting Unicode symbol for lambda

#### Debugger

DrRacket also provides a step-debugger, which could be invoked by clicking `Debug` on the top-right corner. Try it with the following code to see the
underlying interpretation flow.

```racket
(define (fib n)
  (case n
    [(0 1) n]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(fib 2)
```

### [RackUnit](https://docs.racket-lang.org/rackunit/)

RackUnit is a unit-testing framework for Racket. The following is a simple example on its usage, more features could be found on the website linked
above.

```racket
#lang racket
(define (fib n)
  (case n
    [(0 1) n]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))
(module+ test
  (require rackunit)
  (check equal? (fib 1) 2) ; wrong test case
  (check equal? (fib 2) 1)
)
```

You can run the tests using `raco test fib.rkt`. (Assuming the above snippet is saved in a file named "fib.rkt".) The output should be something like:

```racket
raco test: (submod "fib.rkt" test)
--------------------
FAILURE
name:       check
expression: (check = (fib 1) 2)
params:     (#<procedure:=> 1 2)

Check failure
--------------------
1/2 test failures
```

## 1. [Pythagorean triple](https://en.wikipedia.org/wiki/Pythagorean_triple)

Print Pythagorean triples in the following format:

```
a^2 + b^2 = c^2
```
for all `a`, `b`, `c` satisfying `1 <= a <= 20`, `a <= b <=20`, and `b <= c <= 20`, sorted by `a` in ascending order.

### Hints:

This [link](http://docs.racket-lang.org/guide/for.html#%28part._for_and_for_%29) on `for` and `for*` could be useful.

This [link](http://docs.racket-lang.org/reference/Writing.html) contains information on printing formatted string to standard output (stdout).

### Self-assessment

The expected output should be something like:

```
3^2 + 4^2 = 5^2
5^2 + 12^2 = 13^2
6^2 + 8^2 = 10^2
8^2 + 15^2 = 17^2
9^2 + 12^2 = 15^2
```

## 2. Permutation

Write a function `permutation`, which accepts a list, and returns all permutations of the list.

### Hints

There's an Erlang version implementation in the [lecture slides](http://www.it.uu.se/edu/course/homepage/avfunpro/ht15/erlang-part-1.pdf), which you may find inspiring.

List comprehension in Racket could be consulted [here](http://docs.racket-lang.org/guide/for.html#%28part._for_list_and_for__list%29)

### Self-assessment

Some test cases using RackUnit:

```racket
(define (permutation l)
  ...)

(module+ test
  (require rackunit)
  (check equal? (permutation '()) '(()))
  (check equal? (permutation '(1)) '((1)))
  (check equal? (permutation '(1 2)) '((1 2) (2 1)))
  (check equal? (permutation '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
)
```

## 3. QuickSort

Implement QuickSort in Racket.

### Hint

The standard functions related to lists could be found [here](http://docs.racket-lang.org/reference/pairs.html).

## 4. [Lambda application](http://www.scheme.com/tspl4/start.html#g21)

Determine the values of the expressions below without using the interpreter.

```racket
(let  ([f  (lambda  (x) x)])
  (f 'a))

(let  ([f  (lambda  x x)])
  (f 'a))

(let  ([f  (lambda  (x . y) x)])
  (f 'a))

(let  ([f  (lambda  (x . y) y)])
  (f 'a))
```