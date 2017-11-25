Henrik Arro

----------------------------------------------------------------------
Regarding magic.erl:

The strategy used in list_of_spells_next/2 is very simple: delete,
replace or insert a single spell in the list. This works relatively
well, it *does* get stuck sometimes, but running a few hundred test
cycles is normally not a problem.

There is a run_tests/1 helper function that runs a given number of
test cycles.
