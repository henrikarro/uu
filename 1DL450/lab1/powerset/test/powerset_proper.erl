-module(powerset_proper).

-include_lib("proper/include/proper.hrl").

prop_size_is_exponential() ->
    ?FORALL(L, small_lists(), size_is_exponential(L, powerset:powerset(sets:from_list(L)))).

prop_union_is_original_set() ->
    ?FORALL(L, small_lists(), union_is_original_set(L, powerset:powerset(sets:from_list(L)))).

small_lists() ->
    ?SUCHTHAT(L, list(int()), length(L) < 20).

size_is_exponential(L, PS) ->
    sets:size(PS) =:= round(math:pow(2, length(remove_duplicates(L)))).

union_is_original_set(L, PS) ->
    lists:sort(remove_duplicates(L)) =:=
        lists:sort(remove_duplicates(lists:append(sets:to_list(PS)))).

remove_duplicates(L) ->
    sets:to_list(sets:from_list(L)).
