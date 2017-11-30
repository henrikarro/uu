# Haskell lab

## 0. Warmup

### Hoogle

[Hoogle](https://www.haskell.org/hoogle/) is the search engine for Haskell functions. It supports searching by function names and types.

For example, you plan to use `parMap`, but forget its type. You could just type `parMap` in Hoogle, and the search result would look like:

```
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
```

We could search using types as well. Say you would like to double each integer in a list. In other words, you would like a function whose type
should be something like: `[Int] -> (Int -> Int) -> [Int]`. By search that on Hoogle, a list of candidates are returned, and `map` is the first
candidate.

```
map :: (a -> b) -> [a] -> [b]
```

`map` doesn't have the exact types you need, but it could get the job done as well. Here, we see that Hoogle doesn't just do plain text searching.

### QuickCheck

Seem [An introduction to QuickCheck testing](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) for QuickCheck in Haskell.

### Conditional monad 

In the lecture, calculating bounded pythagorian triples using list comprehension is presented. Here's the infinite list using monad and laziness. Note that the conditional execution is achieved using [guard](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad.html#v:guard).

```haskell
pythags :: [(Int, Int, Int)]
pythags = do
 z <- [1..]
 y <- [1..z]
 x <- [1..y]
 guard $ x^2 + y^2 == z^2
 return (x, y, z)
```

## 1. Standard Input (stdin)

Write a Haskell program `stdin.hs` that accepts a list of integers separated by space and prints a list containing the same integers. You may find
[getLine](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:getLine) useful. For example:

```
$ echo 1 2 3 4 | runghc stdin.hs
[1,2,3,4]
```

## 2. Depth-First Search

Implement the problem 2 in [lab1](https://gist.github.com/kostis/f67aa7e4d1e63aa344ac) in Haskell. See Warmup section for how to use QuickCheck in
Haskell.