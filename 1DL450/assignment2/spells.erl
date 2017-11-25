-module(spells).
-export([new_character/0, spells/0, cast_spells/2, sum_attr/1]).

-record(attr, {strength     = 0 :: integer(),
               constitution = 0 :: integer(),
               defense      = 0 :: integer(),
               dexterity    = 0 :: integer(),
               intelligence = 0 :: integer(),
               charisma     = 0 :: integer(),
               wisdom       = 0 :: integer(),
               willpower    = 0 :: integer(),
               perception   = 0 :: integer(),
               luck         = 0 :: integer()}).

-type attr() :: #attr{}.
-type spell() :: {atom(), fun((attr()) -> attr())}.
-type state() :: map().

%% -----------------------
%% Spell and Character API
%% -----------------------

-spec new_character() -> attr().
new_character() ->
  #attr{strength     = 5,
        constitution = 5,
        defense      = 5,
        dexterity    = 5,
        intelligence = 5,
        charisma     = 5,
        wisdom       = 5,
        willpower    = 5,
        perception   = 5,
        luck         = 5}.

-spec simple_spells() -> list(attr()).
simple_spells() ->
  [{iron_reflexes, #attr{strength = 5, constitution = -2, dexterity = -3}},
   {mental_defence, #attr{defense = 4, willpower = -4}},
   {knowledge_through_smartness, #attr{intelligence = -3, charisma = 1, wisdom = 1, perception = 1}},
   {armour_up, #attr{strength = 1,  constitution = 2, defense = 2,  willpower = -3}},
   {weak_leadership, #attr{intelligence = 1,  charisma = 1, wisdom = 1,  willpower = -3}},
   {stupid_luck, #attr{strength = 1,  intelligence = -3, luck = 2}},
   {distributed_will, #attr{intelligence = 1,  charisma = 1, wisdom = 1, willpower = -4, perception = 1}},
   {unlucky_cunning, #attr{charisma = 2, perception = 1, luck = -3}},
   {balanced_strength, #attr{strength = 2,  constitution = -2}},
   {balanced_constitution, #attr{constitution = 2,  defense = -2}},
   {balanced_defense, #attr{defense = 2,  dexterity = -2}},
   {balanced_dexterity, #attr{dexterity = 2,  intelligence = -2}},
   {strong_minded, #attr{strength = -1, constitution = -1, defense = -1, dexterity = -1, intelligence = -1,
         charisma = -1, wisdom = -1, willpower = 10, perception = -1, luck = -1}},
   {balanced_intelligence, #attr{intelligence = 2,  charisma = -2}},
   {balanced_charisma, #attr{charisma = 2,  wisdom = -2}},
   {balanced_wisdom, #attr{wisdom = 2,  willpower = -2}},
   {balanced_willpower, #attr{willpower = 2,  perception = -2}},
   {balanced_perception, #attr{perception = 2,  luck = -2}},
   {balanced_luck, #attr{strength = -2,  luck = 2}},
   {unlucky_power, #attr{strength = 5,  luck = -8}}].

create_simple_spell(Diff) ->
  fun (Attrs, State = #{karma := K}) ->
      NA = Attrs#attr{strength     = Attrs#attr.strength     + calc_spell_eff(Diff#attr.strength, K),
                      constitution = Attrs#attr.constitution + calc_spell_eff(Diff#attr.constitution, K),
                      defense      = Attrs#attr.defense      + calc_spell_eff(Diff#attr.defense, K),
                      dexterity    = Attrs#attr.dexterity    + calc_spell_eff(Diff#attr.dexterity, K),
                      intelligence = Attrs#attr.intelligence + calc_spell_eff(Diff#attr.intelligence, K),
                      charisma     = Attrs#attr.charisma     + calc_spell_eff(Diff#attr.charisma, K),
                      wisdom       = Attrs#attr.wisdom       + calc_spell_eff(Diff#attr.wisdom, K),
                      willpower    = Attrs#attr.willpower    + calc_spell_eff(Diff#attr.willpower, K),
                      perception   = Attrs#attr.perception   + calc_spell_eff(Diff#attr.perception, K),
                      luck         = Attrs#attr.luck         + calc_spell_eff(Diff#attr.luck, K)},
      {NA, State#{karma=> max(0, K - 1)}}
  end.

calc_spell_eff(V, K) ->
  case V>0 of
    true -> trunc(V * (K /100));
    false -> V
  end.

-spec spells() -> list(spell()).
spells() ->
  [{Name, create_simple_spell(Diff)} || {Name, Diff} <- simple_spells()] ++
    [{midnight_ritual, fun spell1/2},
     {lunch_ritual, fun spell2/2},
     {twilight_ritual, fun spell3/2}].

spell1(Attrs, State = #{karma:=K}) ->
  NA = case sum_attr(Attrs) < 70 of
    true -> Attrs#attr{strength     = Attrs#attr.strength     + 1,
                       constitution = Attrs#attr.constitution + 1,
                       defense      = Attrs#attr.defense      + 1,
                       dexterity    = Attrs#attr.dexterity    + 1,
                       intelligence = Attrs#attr.intelligence + 1,
                       charisma     = Attrs#attr.charisma     + 1,
                       wisdom       = Attrs#attr.wisdom       + 1,
                       willpower    = Attrs#attr.willpower    + 1,
                       perception   = Attrs#attr.perception   + 1,
                       luck         = Attrs#attr.luck         + 1};
    false -> Attrs#attr{strength     = Attrs#attr.strength     - 1,
                        constitution = Attrs#attr.constitution - 1,
                        defense      = Attrs#attr.defense      - 1,
                        dexterity    = Attrs#attr.dexterity    - 1,
                        intelligence = Attrs#attr.intelligence - 1,
                        charisma     = Attrs#attr.charisma     - 1,
                        wisdom       = Attrs#attr.wisdom       - 1,
                        willpower    = Attrs#attr.willpower    - 1,
                        perception   = Attrs#attr.perception   - 1,
                        luck         = Attrs#attr.luck         - 1}
  end,
  {NA, State#{karma=> max(0, K - 1)}}.

spell2(Attrs, State = #{karma:=K}) ->
  NA = case K > 90 of
    true -> Attrs#attr{strength     = Attrs#attr.strength     + 1,
                       constitution = Attrs#attr.constitution + 1,
                       defense      = Attrs#attr.defense      + 1,
                       dexterity    = Attrs#attr.dexterity    + 1,
                       intelligence = Attrs#attr.intelligence + 1,
                       charisma     = Attrs#attr.charisma     + 1,
                       wisdom       = Attrs#attr.wisdom       + 1,
                       willpower    = Attrs#attr.willpower    + 1,
                       perception   = Attrs#attr.perception   + 1,
                       luck         = Attrs#attr.luck         + 1};
    false -> Attrs#attr{strength     = Attrs#attr.strength     - 1,
                        constitution = Attrs#attr.constitution - 1,
                        defense      = Attrs#attr.defense      - 1,
                        dexterity    = Attrs#attr.dexterity    - 1,
                        intelligence = Attrs#attr.intelligence - 1,
                        charisma     = Attrs#attr.charisma     - 1,
                        wisdom       = Attrs#attr.wisdom       - 1,
                        willpower    = Attrs#attr.willpower    - 1,
                        perception   = Attrs#attr.perception   - 1,
                        luck         = Attrs#attr.luck         - 1}
  end,
      {NA, State#{karma=> max(0, K - 1)}}.

spell3(OldAttrs, State = #{triggers:=T, karma:=K}) ->
  TriggerFun = fun (_Attrs,  #{karma:=KK}) when KK > 50 -> not_triggered;
                   (Attrs, S) ->
                   NA = Attrs#attr{strength     = Attrs#attr.strength     - 1,
                                   constitution = Attrs#attr.constitution - 1,
                                   defense      = Attrs#attr.defense      - 1,
                                   dexterity    = Attrs#attr.dexterity    - 1,
                                   intelligence = Attrs#attr.intelligence - 1,
                                   charisma     = Attrs#attr.charisma     - 1,
                                   wisdom       = Attrs#attr.wisdom       - 1,
                                   willpower    = Attrs#attr.willpower    - 1,
                                   perception   = Attrs#attr.perception   - 1,
                                   luck         = Attrs#attr.luck         - 1},
                   {ok, {NA, S}}
                 end,
  {OldAttrs, State#{triggers => [TriggerFun|T], karma=> max(0, K - 1)}}.

-spec cast_spell(attr(), spell(), state()) -> {attr(), state()}.
cast_spell(Attrs, {_, Spell}, State) ->
  {RawNewAttrs, RawNewState= #{triggers:=T}} = Spell(Attrs, State),
  {NewAttrs, NewState} = process_triggers(RawNewAttrs, RawNewState, T, []),
  if
    NewAttrs#attr.strength < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.constitution < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.defense < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.dexterity < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.intelligence < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.charisma < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.wisdom < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.willpower < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.perception < 0 -> penalty_attr(Attrs, NewAttrs, State);
    NewAttrs#attr.luck < 0 -> penalty_attr(Attrs, NewAttrs, State);
    true -> {NewAttrs, NewState}
  end.

process_triggers(Attrs, State, [], Untriggered) ->
  {Attrs, State#{triggers=>lists:reverse(Untriggered)}};
process_triggers(Attrs, State, [Tr|Trs], Untriggered) ->
  case Tr(Attrs, State) of
    {ok, {NA, NS}} ->
      process_triggers(NA, NS, Trs, Untriggered);
    not_triggered ->
      process_triggers(Attrs, State, Trs, [Tr|Untriggered])
    end.

%% Penalty for wrongly casted spells
penalty_attr(Attrs, _NewAttrs, State = #{karma:=K}) ->
  {Attrs, State#{karma => max(K - 2, 0)}}.

cast_spells(Attrs, [], _) -> Attrs;
cast_spells(Attrs, [Spell | LeftSpells], State) ->
  {NA, NS} = cast_spell(Attrs, Spell, State),
  cast_spells(NA, LeftSpells, NS).

cast_spells(Attrs, Spells) ->
  State = #{karma => 100, triggers=>[]},
  cast_spells(Attrs, Spells, State).

sum_attr(Attrs) ->
  Attrs#attr.strength + Attrs#attr.constitution +
    Attrs#attr.defense + Attrs#attr.dexterity +
    Attrs#attr.intelligence + Attrs#attr.charisma +
    Attrs#attr.wisdom + Attrs#attr.willpower +
    Attrs#attr.perception + Attrs#attr.luck.
