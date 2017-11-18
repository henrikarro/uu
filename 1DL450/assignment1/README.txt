Henrik Arro

----------------------------------------------------------------------
Regarding calc_pi:

I have been sloppy with rounding errors, for example, the total number
of simulation steps may not be exaclty N when they are spread out over
the workers.

I do not think this matters in practice, since we are calculating an
approximation anyway, and N will usually be quite large.

----------------------------------------------------------------------
Regarding bughunt:

Please see the comment near the top of bughunt.erl explaining why I
had to comment out the export of the properties.

I don't understand this problem, but I hope you can find a way to run
the tests, for example using "c(bughunt, [export_all])".

There is a helper function, bughunt:run_tests/0, that checks all the
50 implementations. It shows that implementations 27, 35, 37 and 39
are correct, while the other 46 implementations are flawed.

Personally, I think the four 'correct' implementations actually are
not correct, since they allow an expression with non-integer vectors
like '[foo]'. :-)
