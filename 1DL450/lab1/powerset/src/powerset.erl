-module(powerset).

-export([powerset/1]).

powerset(Set) ->
    sets:from_list(powerset_list(sets:to_list(Set))).

-spec powerset_list([T]) -> [[T]].
powerset_list([]) -> [[]];
powerset_list([E|T]) ->
    PT = powerset_list(T),
    PT ++ f(E, PT).
                     
f(E, T) ->
    [[E|X] || X <- T].
