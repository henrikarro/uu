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

----------------------------------------------------------------------
Regarding relations.rkt

I "cheat" by letting the normal Racket evaluator do most of the work:

- #%top is redefined to return a global relation definition.

- In restrict, I create a namespace where column names are bound
to the row's values, and then simply eval the condition. This means
that restrict accepts much more complex conditions than the ones
specified in the assignment description. For example, you can do

(restrict pers (salary . < . (* (string-length name) 1000)))

to find low-paid people with long names. :-)

I consider this a feature and not a bug.

(To be honest, I have actually been trying to find a way to create
a namespace with only the exported names from relations.rkt, but
have not found any. So instead I create a "normal" namespace and
define "=" and "!=" appropriately.)
