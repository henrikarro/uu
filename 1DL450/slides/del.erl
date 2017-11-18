-module(del).
-export([delete/2]).

-include_lib("proper/include/proper.hrl").

delete(_, []) -> [];
delete(X, [H|T]) ->
  case X =:= H of
    true -> T;
    false -> [H|delete(X, T)]
  end.

prop_delete() ->
  %% ?FORALL({E, L}, {float(), list(float())},
  ?FORALL({E, L}, {integer(), list(integer())},
	  lists:member(E, delete(E, L)) =:= false).
