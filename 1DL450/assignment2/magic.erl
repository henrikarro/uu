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
    frequency([
               {1, list_of_spells()},
               {99, add_remove_or_replace_random_spell(PreviousSpells)}
              ]).

add_remove_or_replace_random_spell(PreviousSpells) ->
    ?LET(NextSpell, spell(),
         begin
             R = rand:uniform(),
             Spells =
                 if
                     length(PreviousSpells) > 0 andalso R < 0.3 ->
                         delete(PreviousSpells, rand:uniform(length(PreviousSpells)));
                     length(PreviousSpells) > 0 andalso R < 0.7 ->
                         replace(PreviousSpells, rand:uniform(length(PreviousSpells)), NextSpell);
                     length(PreviousSpells) > 0 ->
                         insert(PreviousSpells, rand:uniform(length(PreviousSpells) + 1), NextSpell);
                     true ->
                         [NextSpell]
                 end,

%             io:format("~w ~w~n", [length(PreviousSpells), length(Spells)]),
             Spells
         end).

%% @doc
%% Deletes the element at position N (1 &#8804; N &#8804; length(L)) in list L.
-spec delete([any(), ...], pos_integer()) -> [any()].
delete(L, N) ->
    lists:delete(lists:nth(N, L), L).

%% @doc
%% Inserts E at position N (1 &#8804; N &#8804; length(L)+1) in list L.
-spec insert([any()], pos_integer(), any()) -> [any(), ...].
insert(L, 1, E) -> [E|L];
insert([X|Xs], N, E) -> [X|insert(Xs, N - 1, E)].

%% @doc
%% Replaces element at position N (1 &#8804; N &#8804; length(L)) in list L with E.
-spec replace([any(), ...], pos_integer(), any()) -> [any(), ...].
replace([_|Xs], 1, E) -> [E|Xs];
replace([X|Xs], N, E) -> [X|replace(Xs, N - 1, E)].

%% @doc
%% Runs N test cycles with 10,000 tests each, returning `ok' if all
%% test cycles successfully find a counterexample, `fail' otherwise.
-spec run_tests(non_neg_integer()) -> {ok | fail}.
run_tests(0) -> ok;
run_tests(N) ->
    io:format("Running test ~w~n", [N]),
    case proper:quickcheck(prop_spells_targeted(), [{numtests, 10000}, quiet]) of
        true ->
            fail;
        false ->
            run_tests(N - 1)
    end.
