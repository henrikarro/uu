-module(magic).
-include_lib("proper/include/proper.hrl").

-export([run_tests/1]).

%% -------------------------
%% Properties and Generators
%% -------------------------
list_of_spells() ->
  list(proper_types:noshrink(oneof(spells:spells()))).

prop_spells() ->
  ?FORALL(Spells, list_of_spells(),
          begin
            InitialAttr = spells:new_character(),
            BuffedAttr = spells:cast_spells(InitialAttr, Spells),
            SumAttr = spells:sum_attr(BuffedAttr),
            SumAttr < 3 * spells:sum_attr(InitialAttr)
          end).

prop_spells_targeted() ->
  ?FORALL_SA(Spells, ?TARGET(list_of_spells_sa()),
             begin
               InitialAttr = spells:new_character(),
               BuffedAttr = spells:cast_spells(InitialAttr, Spells),
               SumAttr = spells:sum_attr(BuffedAttr),
               ?MAXIMIZE(SumAttr),
               SumAttr < 3 * spells:sum_attr(InitialAttr)
             end).

list_of_spells_sa() ->
  #{first => list_of_spells(),
    next => fun list_of_spells_next/2}.

spell() ->
    oneof(spells:spells()).

list_of_spells_next(PreviousSpells, _Temperature) ->
    ?LET(NextSpell, spell(),
         begin
             R = rand:uniform(),
             Spells =
                 if
                     length(PreviousSpells) > 0 andalso R < 0.3 ->
                         lists:delete(lists:nth(rand:uniform(length(PreviousSpells)), PreviousSpells), PreviousSpells);
                     length(PreviousSpells) > 0 andalso R < 0.7 ->
                         replace(PreviousSpells, rand:uniform(length(PreviousSpells)), NextSpell);
                     length(PreviousSpells) > 0 ->
                         insert(PreviousSpells, rand:uniform(length(PreviousSpells)), NextSpell);
                     true ->
                         [NextSpell]
                 end,
             Spells
         end).

%% @doc
%% Inserts E at position N in list L.
-spec insert(list(any()), non_neg_integer(), any()) -> list(any()).
insert([], _, E) -> [E];
insert(L, 0, E) -> [E|L];
insert([X|Xs], N, E) -> [X|insert(Xs, N - 1, E)].

%% @doc
%% Replaces element at position N in list L with E.
-spec replace(list(any()), non_neg_integer(), any()) -> list(any()).
replace([], _, _) -> [];
replace([_|Xs], 0, E) -> [E|Xs];
replace([X|Xs], N, E) -> [X|replace(Xs, N - 1, E)].

%% @doc
%% Runs N test cycles with 10,000 tests each, returning `ok' if all
%% test cycles successfully find a counterexample, `fail' otherwise.
-spec run_tests(non_neg_integer()) -> {ok | fail}.
run_tests(0) -> ok;
run_tests(N) ->
    case proper:quickcheck(prop_spells_targeted(), [{numtests, 10000}, quiet]) of
        true ->
            fail;
        false ->
            run_tests(N - 1)
    end.
