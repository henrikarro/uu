Henrik Arro

----------------------------------------------------------------------
Regarding magic.erl:

The strategy used in list_of_spells_next/2 is very simple: delete,
replace or insert a single spell in the list, resetting to a new list
of spells every now and then to get out of local maxima. This works
relatively well, it *does* get stuck sometimes, but running a few
hundred test cycles is normally not a problem.

There is a run_tests/1 helper function that runs a given number of
test cycles.

----------------------------------------------------------------------
Regarding dice.erl/dice.rkt

The algorithm is the same in both programs: find all the nodes that
can be reached by the number of steps given by the next die, stopping
if we reach the winning position, or if we have seen the position
before (in a cycle).

The dice and the positions that we have already seen are stored as
global data in both programs. In dice.erl, a stateful process holds
the state, while in dice.rkt, a class represents the state and a
global singleton object is created.
