Henrik Arro

----------------------------------------------------------------------
Regarding dice.hs

I have not bothered to speed up the algorithm, so it will still fail
on the 12th test case, with the very "wide" tree with many high-degree
vertices.

Besides the normal dice function, I created an alternative version
called stDice that uses the State monad to keep the GameState. I was
hoping that hiding the state would make the code clearer. However,
the do notation in Haskell is very limited in that you can not refer
to a monadic function directly in an expression, you can only get its
value using '<-'. For example, you can do

    currentDieNumber <- stGetCurrentDieNumber

and then use the value of currentDieNumber, but you cannot use the
function stGetCurrentDieNumber in some other expression. This means
that you must create a lot of "temporary variables" which makes the
code more difficult to follow. The fact that you must handle "pure"
(i.e., non-monadic) functions differently, using let instedad of '<-',
only makes matters worse.

In short, I don't think the stDice function is any more clear than
the original, probably quite the opposite. I must admit that I'm not
convinced that handling side effects using monads is a good idea.

----------------------------------------------------------------------
Regarding my_lazy.hs

This was fun!

By drawing the list as cons cells, it was easy to see how you had to
refer back to the root to get an increasing number of elements to
copy, but it still took a good night's sleep before I realized that
I could use a counter and the take function to implement the out
sequence.

----------------------------------------------------------------------
Regarding indexing.hs

I have refactored the program to build the separate indices using
runParIO (buildIndices function), and not join the indices into one,
but instead do a parallel search with "using parList rseq" (in the
searchForWords function).

This seems to utilize at least the four cores on my laptop quite well,
according to ThreadScope. It also seems to speed up the program using
a single core.
