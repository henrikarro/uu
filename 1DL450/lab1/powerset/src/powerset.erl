-module(powerset).

-export([powerset/1]).

%% @doc Gives the powerset of the given set, i.e., a set with all subsets of the set.
-spec powerset(sets:set(T)) -> sets:set(sets:set(T)).
powerset(Set) ->
    sets:from_list(powerset_list(sets:to_list(Set))).

-spec powerset_list([T]) -> [[T], ...].
powerset_list([]) -> [[]];
powerset_list([E|T]) ->
    PT = powerset_list(T),
    PT ++ f(E, PT).

%% @doc Adds an element E to each list in T.
%%
% %% @reference See <a href="https://en.wikipedia.org/wiki/Power_set#Algorithms">
% %% Wikipedia</a> for more information.
-spec f(T, [[T], ...]) -> [[T, ...]].
f(E, T) ->
    [[E|X] || X <- T].
