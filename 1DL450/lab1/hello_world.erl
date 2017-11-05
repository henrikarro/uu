-module(hello_world).

-export([run/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

run() ->
    io:format("~s~n", ["Hello World!"]),
    io:format("Factorial of 42 is ~p~n", [fact(42)]).

fact(0) -> 1;
fact(X) when X > 0 ->
    X * fact(X - 1).

fact_test_() ->
    [?_assertEqual(R, fact(N)) || {N, R} <- [{0, 1}, {1, 1}, {4, 24}]].

prop_divisible_by_all_up_to() ->
    ?FORALL(N, non_neg_integer(), divisible_by_all_up_to(N, fact(N))).

divisible_by_all_up_to(N, F) ->
    lists:all(fun(X) -> F rem X =:= 0 end, lists:seq(1, N)).
