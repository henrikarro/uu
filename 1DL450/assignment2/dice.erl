-module(dice).

-export([dice/3]).

-define(STATE_SERVER, state_server).

-type die() :: 1 | 2 | 3 | 4 | 5 | 6.

-type game_node() :: pos_integer().

-spec dice(game_node(), [{game_node(), game_node()}, ...], [die()]) -> integer().
dice(N, Nodes, Dice) ->
    spawn_state_server(Dice),
    Graph = build_graph(Nodes),
    dice(Graph, N, [1], 0).

-spec dice(digraph:graph(), game_node(), [game_node()], non_neg_integer()) -> integer().
dice(_Graph, _WinningNode, [], _MoveNumber) ->
    -1;
dice(Graph, WinningNode, StartNodes, MoveNumber) ->
    case lists:any(fun(Node) -> Node =:= WinningNode end, StartNodes) of
        true ->
            MoveNumber;
        false ->
            case has_seen_position_before(current_die_number(), StartNodes) of
                true ->
                    -1;
                false ->
                    add_to_positions_seen(current_die_number(), StartNodes),
                    Die = next_die(),
                    case Die =:= -1 of
                        true ->
                            -1;
                        false ->
                            EndNodes = traverse_to_depth(Graph, StartNodes, Die),
                            dice(Graph, WinningNode, EndNodes, MoveNumber + 1)
                    end
            end
    end.

%% @doc
%% Creates a `digraph:graph()´ from a list of node pairs.
-spec build_graph([{game_node(), game_node()}]) -> digraph:graph().
build_graph(Nodes) ->
    build_graph(Nodes, digraph:new()).

%% @doc
%% Updates the Graph from a list of node pairs. Each node in the list
%% becomes a vertex, with an edge N1->N2 between the nodes in each pair
%% `{N1, N2}'.
-spec build_graph([{game_node(), game_node()}], digraph:graph()) -> digraph:graph().
build_graph([], Graph) ->
    Graph;
build_graph([{N1,N2}|Nodes], Graph) ->
    V1 = digraph:add_vertex(Graph, N1),
    V2 = digraph:add_vertex(Graph, N2),
    digraph:add_edge(Graph, V1, V2),
    build_graph(Nodes, Graph).

%% @doc
%% Traverses the graph to the given depth starting from any of the start nodes,
%% returning a list with the nodes that can be reached.
traverse_to_depth(_Graph, [], _Depth) -> [];
traverse_to_depth(_Graph, StartNodes, 0) -> StartNodes;
traverse_to_depth(Graph, [StartNode|StartNodes], Depth) ->
    N1 = traverse_to_depth(Graph, digraph:out_neighbours(Graph, StartNode), Depth - 1),
    N2 = traverse_to_depth(Graph, StartNodes, Depth),
    lists:usort(N1 ++ N2).

%%%===================================================================
%%% State Server
%%%===================================================================
%
% I decided to use a stateful process to keep track of the dice throws
% and of the positions that we have already encountered when searching
% the graph.
%
% This is mainly for fun, but it does give an easy way to implement the
% cyclic nature of the dice throws; the function next_die/0 returns
% the current die value, and updates the state to the next die,
% starting from the beginning if we have reached the end.
%
% The state also contains a list of the positions that we have already
% seen when traversing the graph. A position is a combination of a die
% number (which die throw in the list of available dice we are currently
% using) and a number of nodes that we are currently investigating.
% This is used to detect cycles in the search, so that we can abort
% when appropriate.
%---------------------------------------------------------------------

-type die_number() :: pos_integer().

-record(dice_information, {current_die_number :: die_number(),
                           dice :: [die()]}).
-type dice_information() :: #dice_information{}.

-record(position_seen, {die_number :: die_number(),
                        nodes :: [game_node(), ...]}).
-type position_seen() :: #position_seen{}.

-record(state, {dice_information :: dice_information(),
                positions_seen :: [position_seen()]}).
-type state() :: #state{}.

-spec spawn_state_server([die()]) -> true.
spawn_state_server(Dice) ->
    case whereis(?STATE_SERVER) of
        undefined ->
            ok;
        _OldPid ->
            ?STATE_SERVER ! {dice, self(), kill},
            receive
                goodbye ->
                    ok
            after 1000 ->
                    ok
            end
    end,
    Pid = spawn(fun() -> state_loop(#state{dice_information=#dice_information{current_die_number=1,
                                                                              dice=Dice},
                                           positions_seen=[]}) end),
    register(?STATE_SERVER, Pid).

-spec state_loop(state()) -> goodbye.
state_loop(State) ->
    receive
        {dice, Pid, current_die_number} ->
            Pid ! {current_die_number, current_die_number(State)},
            state_loop(State);
        {dice, Pid, next_die} ->
            Pid ! {current_die, current_die(State)},
            NewState = update_state_to_next_die(State),
            state_loop(NewState);
        {dice, Pid, {has_seen_position_before, CurrentDieNumber, Nodes}} ->
            Pid ! {has_seen_position_before, has_seen_position_before(State, CurrentDieNumber, Nodes)},
            state_loop(State);
        {dice, _Pid, {add_to_positions_seen, CurrentDieNumber, Nodes}} ->
            NewState = update_state_with_positions_seen(State, CurrentDieNumber, Nodes),
            state_loop(NewState);
        {dice, Pid, kill} ->
            Pid ! goodbye
    end.

-spec current_die_number(state()) -> die_number().
current_die_number(#state{dice_information=#dice_information{current_die_number=CurrentDie}}) ->
    CurrentDie.

-spec current_die(state()) -> die().
current_die(#state{dice_information=#dice_information{current_die_number=CurrentDieNumber, dice=Dice}}) ->
    case length(Dice) =:= 0 of
        true ->
            -1;
        false ->
            lists:nth(CurrentDieNumber, Dice)
    end.

-spec update_state_to_next_die(state()) -> state().
update_state_to_next_die(State = #state{dice_information=#dice_information{current_die_number=CurrentDieNumber, dice=Dice}}) ->
    case CurrentDieNumber < length(Dice) of
        true ->
            State#state{dice_information=#dice_information{current_die_number=CurrentDieNumber + 1, dice=Dice}};
        false ->
            State#state{dice_information=#dice_information{current_die_number=1, dice=Dice}}
    end.

-spec has_seen_position_before(state(), die_number(), [game_node()]) -> boolean().        
has_seen_position_before(#state{positions_seen = PositionsSeen}, CurrentDieNumber, Nodes) ->
    lists:member(#position_seen{die_number=CurrentDieNumber, nodes=Nodes}, PositionsSeen).

-spec update_state_with_positions_seen(state(), die_number(), [game_node()]) -> state().
update_state_with_positions_seen(State = #state{positions_seen=PositionsSeen}, CurrentDieNumber, Nodes) ->
    State#state{positions_seen=[#position_seen{die_number=CurrentDieNumber,
                                               nodes=Nodes}
                                |PositionsSeen]}.

%%--------------------------------------------------------------------
%% Accessor functions that query or update the state_server
%%--------------------------------------------------------------------

%% @doc
%% Gives the current 
-spec current_die_number() -> die_number().
current_die_number() ->
    ?STATE_SERVER ! {dice, self(), current_die_number},
    receive
        {current_die_number, CurrentDieNumber} ->
            CurrentDieNumber
    end.

-spec next_die() -> die() | -1.
next_die() ->
    ?STATE_SERVER ! {dice, self(), next_die},
    receive
        {current_die, CurrentDie} ->
            CurrentDie
    end.

-spec has_seen_position_before(die_number(), [game_node()]) -> boolean().
has_seen_position_before(CurrentDieNumber, Nodes) ->
    ?STATE_SERVER ! {dice, self(), {has_seen_position_before, CurrentDieNumber, Nodes}},
    receive
        {has_seen_position_before, SeenBefore} ->
            SeenBefore
    end.

-spec add_to_positions_seen(die_number(), [game_node(), ...]) -> ok.
add_to_positions_seen(CurrentDieNumber, Nodes) ->
    ?STATE_SERVER ! {dice, self(), {add_to_positions_seen, CurrentDieNumber, Nodes}},
    ok.
    
%%%===================================================================
%%% EUnit Test Cases
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

dice_test_() ->
    [
     ?_assertEqual(0, dice(1, [{1,2}], [])),
     ?_assertEqual(-1, dice(2, [{1,2}], [])),
     ?_assertEqual(1, dice(2, [{1,2}], [1])),
     ?_assertEqual(-1, dice(2, [{1,2}], [2])),
     ?_assertEqual(-1, dice(2, [{1,2}, {2,1}], [2])),
     ?_assertEqual(1, dice(2, [{1,2}, {2,1}], [3]))
    ].

dice_test_cases_from_assignment_description_test_() ->
    [
     ?_assertEqual(2, dice:dice(3, [{1,2}, {2,1}, {2,3}, {3,2}], [3,5])),
     ?_assertEqual(3, dice:dice(4, [{1,2}, {2,3}, {3,4}], [1])),
     ?_assertEqual(-1, dice:dice(3, [{1,2}, {2,3}], [4,2,6]))
    ].

build_empty_graph_test() ->
    G = build_graph([]),
    ?assertEqual([], digraph:vertices(G)),
    ?assertEqual([], digraph:edges(G)).

build_graph_with_one_edge_test() ->
    G = build_graph([{1,2}]),
    ?assertEqual([1,2], lists:sort(digraph:vertices(G))),
    ?assertEqual([2], digraph:out_neighbours(G, 1)),
    ?assertEqual([], digraph:out_neighbours(G, 2)).

build_graph_with_two_edges_test() ->
    G = build_graph([{1,2}, {2,1}]),
    ?assertEqual([1,2], lists:sort(digraph:vertices(G))),
    ?assertEqual([2], digraph:out_neighbours(G, 1)),
    ?assertEqual([1], digraph:out_neighbours(G, 2)).
    
build_graph_with_four_edges_test() ->
    G = build_graph([{1,2}, {2,1}, {2,3}, {3,2}]),
    ?assertEqual([1,2,3], lists:sort(digraph:vertices(G))),
    ?assertEqual([2], digraph:out_neighbours(G, 1)),
    ?assertEqual([1,3], lists:sort(digraph:out_neighbours(G, 2))),
    ?assertEqual([2], digraph:out_neighbours(G, 3)).

traverse_to_depth_graph_1_test() ->
    G = build_graph([{1,2}, {2,1}, {2,3}, {3,2}]),
    % Search starting from 1
    ?assertEqual([1], traverse_to_depth(G, [1], 0)),
    ?assertEqual([2], traverse_to_depth(G, [1], 1)),
    ?assertEqual([1,3], traverse_to_depth(G, [1], 2)),
    ?assertEqual([2], traverse_to_depth(G, [1], 3)),
    ?assertEqual([1,3], traverse_to_depth(G, [1], 4)),
    ?assertEqual([2], traverse_to_depth(G, [1], 5)),
    % Search starting from 2
    ?assertEqual([2], traverse_to_depth(G, [2], 0)),
    ?assertEqual([1,3], traverse_to_depth(G, [2], 1)),
    ?assertEqual([2], traverse_to_depth(G, [2], 2)),
    ?assertEqual([1,3], traverse_to_depth(G, [2], 3)),
    ?assertEqual([2], traverse_to_depth(G, [2], 4)),
    ?assertEqual([1,3], traverse_to_depth(G, [2], 5)),
    % Search starting from 3
    ?assertEqual([3], traverse_to_depth(G, [3], 0)),
    ?assertEqual([2], traverse_to_depth(G, [3], 1)),
    ?assertEqual([1,3], traverse_to_depth(G, [3], 2)),
    ?assertEqual([2], traverse_to_depth(G, [3], 3)),
    ?assertEqual([1,3], traverse_to_depth(G, [3], 4)),
    ?assertEqual([2], traverse_to_depth(G, [3], 5)).

traverse_to_depth_graph_2_test() ->
    G = build_graph([{1,2}, {2,3}, {3,4}]),
    % Search starting from 1
    ?assertEqual([1], traverse_to_depth(G, [1], 0)),
    ?assertEqual([2], traverse_to_depth(G, [1], 1)),
    ?assertEqual([3], traverse_to_depth(G, [1], 2)),
    ?assertEqual([4], traverse_to_depth(G, [1], 3)),
    ?assertEqual([], traverse_to_depth(G, [1], 4)),
    % Search starting from 2
    ?assertEqual([2], traverse_to_depth(G, [2], 0)),
    ?assertEqual([3], traverse_to_depth(G, [2], 1)),
    ?assertEqual([4], traverse_to_depth(G, [2], 2)),
    ?assertEqual([], traverse_to_depth(G, [2], 3)),
    % Search starting from 3
    ?assertEqual([3], traverse_to_depth(G, [3], 0)),
    ?assertEqual([4], traverse_to_depth(G, [3], 1)),
    ?assertEqual([], traverse_to_depth(G, [3], 2)),
    % Search starting from 4
    ?assertEqual([4], traverse_to_depth(G, [4], 0)),
    ?assertEqual([], traverse_to_depth(G, [4], 1)).

