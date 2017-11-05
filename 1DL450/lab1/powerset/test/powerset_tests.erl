-module(powerset_tests).

-include_lib("eunit/include/eunit.hrl").

-import(powerset, [powerset/1]).

powerset_test_() ->
    [?_assertEqual([[]], set_to_list(powerset(sets:new()))),
     ?_assertEqual([[], [1]], set_to_list(powerset(sets:from_list([1])))),
     ?_assertEqual([[], [1], [1, 2], [2]], set_to_list(powerset(sets:from_list([1, 2])))),
     ?_assertEqual([[], [1], [1, 2], [1, 2, 3], [1, 3], [2], [2, 3], [3]],
                   set_to_list(powerset(sets:from_list([1, 2, 3]))))].

set_to_list(S) ->
    lists:sort([lists:sort(L) || L <- sets:to_list(S)]).
