-module(road).

-export([days/3, add_interval/2]).

days(Length, Intervals, Goal) ->
    days(Length, Intervals, Goal, [], 0).

days(_Length, [], _Goal, _SoFar, _Day) -> -1;
days(Length, [Interval|Intervals], Goal, IntervalsSoFar, Day) ->
    case is_schedulable(Goal, IntervalsSoFar, 1, Length) of
        true ->
            NewIntervals = add_interval(Interval, IntervalsSoFar),
            days(Length, Intervals, Goal, NewIntervals, Day + 1);
        false -> Day - 1
    end.

add_interval({L1,H1}, []) -> [{L1,H1}];
add_interval({L1,H1}, [{L2,H2}|Intervals]) when H1 < L2 ->
    [{L1,H1},{L2,H2}|Intervals];
add_interval({L1,H1}, [{L2,H2}|Intervals]) when L1 > H2 ->
    [{L2,H2}|add_interval({L1,H1}, Intervals)];
add_interval({L1,H1}, [{L2,H2}|Intervals]) when L1 =< H2 ->
    add_interval({min(L1,L2),max(H1,H2)}, Intervals);
add_interval({L1,H1}, [{L2,H2}|Intervals]) when L1 =< L2 andalso H1 >= H2 ->
    [{L1,H1}|Intervals];
add_interval({L1,H1}, [{L2,H2}|Intervals]) when L1 >= L2 andalso H1 =< H2 ->
    [{L2,H2}|Intervals].
%% add_interval(NewInterval, [Interval|Intervals]) ->
%%     lists:sort([Interval|add_interval(NewInterval, Intervals)]).

is_schedulable(Length, [], Start, End) -> End - Start + 1 >= Length;
is_schedulable(_Length, _Intervals, Start, End) when Start >= End -> false;
is_schedulable(Length, [{L,H}|Intervals], Start, End) when Start < L ->
    L - Start + 1 >= Length orelse is_schedulable(Length, Intervals, H, End);
is_schedulable(Length, [{L,H}|Intervals], Start, End) when Start >= L ->
    is_schedulable(Length, Intervals, H, End).
    

%%--------------------------------------------------------------------
%% EUnit Test Cases
%%--------------------------------------------------------------------
 
-include_lib("eunit/include/eunit.hrl").

days_test_() ->
    [?_assertEqual(2, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 6)),
     ?_assertEqual(-1, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 1))].

add_interval_test_() ->
    [?_assertEqual([{5,10}], add_interval({5,10}, [])),
     ?_assertEqual([{5,10}, {11,15}], add_interval({11,15}, [{5,10}])),
     ?_assertEqual([{5,15}], add_interval({10,15}, [{5,10}])),
     ?_assertEqual([{5,15}], add_interval({7,15}, [{5,10}])),
     ?_assertEqual([{5,10}], add_interval({6,9}, [{5,10}])),
     ?_assertEqual([{5,10}], add_interval({5,10}, [{5,10}])),
     ?_assertEqual([{1,10}], add_interval({1,6}, [{5,10}])),
     ?_assertEqual([{1,10}], add_interval({1,5}, [{5,10}])),
     ?_assertEqual([{1,4},{5,10}], add_interval({1,4}, [{5,10}])),
     ?_assertEqual([{1,20}], add_interval({1,20}, [{1,4},{5,10}])),
     ?_assertEqual([{5,10}, {11,15}], add_interval({5,10}, [{11,15}]))
    ].

%% invert_intervals([], Start, End, Result) when Start < End ->
%%     Result ++ [{Start, End}];
%% invert_intervals([], Start, End, Result) when Start >= End ->
%%     Result;
%% invert_intervals([{L,H}|Intervals], Start, End, Result) when Start < L -> 
%%     invert_intervals(Intervals, H, End, Result ++ [{Start, L}]);
%% invert_intervals([{L,H}|Intervals], Start, End, Result) when Start >= L -> 
%%     invert_intervals(Intervals, H, End, Result).

%% longest_interval([], Result) -> Result;
%% longest_interval([{L,H}|Intervals], Result) when H - L > Result ->
%%     longest_interval(Intervals, H - L);
%% longest_interval([{L,H}|Intervals], Result) when H - L =< Result ->
%%     longest_interval(Intervals, Result).

%% generate_random_intervals(0, _Max, _MaxLength, Intervals) -> Intervals;
%% generate_random_intervals(N, Max, MaxLength, Intervals) ->
%%     generate_random_intervals(N - 1, Max, MaxLength, [generate_random_interval(Max, MaxLength)|Intervals]).

%% generate_random_interval(Max, MaxLength) ->
%%     L = rand:uniform(Max - 1),
%%     H = L + rand:uniform(MaxLength),
%%     case H > Max of
%%         true ->
%%             {L, Max};
%%         false ->
%%             {L, H}
%%     end.

%% benchmark(Fun, ArgList) ->
%%     Rs = [timer:tc(?MODULE, Fun, ArgList) || _ <- lists:seq(1, 100)],
%%     lists:sum([T || {T, _} <- Rs]) / (1000 * length(Rs)).

%% add_intervals(Intervals) -> add_intervals(Intervals, []).

%% add_intervals([], IntervalsSoFar) -> IntervalsSoFar;
%% add_intervals([Interval|Intervals], IntervalsSoFar) ->
%%     add_intervals(Intervals, add_interval(Interval, IntervalsSoFar)).
