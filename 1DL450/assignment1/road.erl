-module(road).

-export([days/3, generate_random_intervals/3, benchmark/3]).

% We export functions called only by PropEr to avoid Dialyzer specializing
% the type specifications to the values used in the properties (e.g., 1000, 100).
% This is a hack, but I don't know a better solution. I guess that normally, the
% PropEr tests would be in a separate test module, and then the problem of
% exporting unnecessary functions is not that big.
-export([verify_result/4, days_alt/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type interval() :: {pos_integer(), pos_integer()}.

-spec days(pos_integer(), [interval()], non_neg_integer()) -> integer().
days(Length, Intervals, Goal) ->
    days(Length, Intervals, Goal, [], 0).

-spec days(pos_integer(), [interval()], non_neg_integer(), [interval()], non_neg_integer()) -> integer().
days(Length, [], Goal, IntervalsSoFar, Day) ->
    case is_schedulable(Goal + 1, IntervalsSoFar, 0, Length) of
        true -> -1;
        false -> Day
    end;
days(Length, [Interval|Intervals], Goal, IntervalsSoFar, Day) ->
    case is_schedulable(Goal + 1, IntervalsSoFar, 0, Length) of
        true ->
            NewIntervals = add_interval(Interval, IntervalsSoFar),
            days(Length, Intervals, Goal, NewIntervals, Day + 1);
        false -> Day
    end.

%% @doc
%% Adds an interval to a list of intervals which is supposed to be sorted
%% by the lower limits, and with no overlapping intervals. The new interval
%% is merged with neighboring intervals if possible, and the resulting
%% interval is inserted in the correct place to keep the resulting list
%% sorted by lower limit.
%%
%% Example: road:add_interval({3,6}, [{1,2},{5,7}]) = [{1,2},{3,7}]
-spec add_interval(interval(), [interval()]) -> [interval(), ...].
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

%% @doc
%% Determines if a task of length Length can be successfully scheduled in
%% a time window defined by Start and End, and with other tasks defined by
%% a list of intervals already scheduled.
-spec is_schedulable(non_neg_integer(), [interval()], pos_integer(), pos_integer()) -> boolean().
is_schedulable(Length, [], Start, End) -> End - Start >= Length;
is_schedulable(_Length, _Intervals, Start, End) when Start >= End -> false;
is_schedulable(Length, [{L,H}|Intervals], Start, End) when Start < L ->
    L - Start >= Length orelse is_schedulable(Length, Intervals, H, End);
is_schedulable(Length, [{L,H}|Intervals], Start, End) when Start >= L ->
    is_schedulable(Length, Intervals, H, End).

%%--------------------------------------------------------------------
%% EUnit Test Cases
%%--------------------------------------------------------------------

days_assignment_example_test_() ->
    [
     ?_assertEqual(-1, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 0)),
     ?_assertEqual(-1, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 1)),
     ?_assertEqual(4, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 2)),
     ?_assertEqual(3, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 3)),
     ?_assertEqual(3, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 4)),
     ?_assertEqual(3, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 5)),
     ?_assertEqual(2, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 6)),
     ?_assertEqual(2, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 7)),
     ?_assertEqual(2, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 24)),
     ?_assertEqual(1, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 25)),
     ?_assertEqual(1, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 26)),
     ?_assertEqual(1, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 29)),
     ?_assertEqual(0, road:days(30, [{1,5},{11,27},{2,14},{18,28}], 30))
    ].

days_test_() ->
    [
     ?_assertEqual(1, road:days(1, [{0,1}], 0)),
     ?_assertEqual(-1, road:days(1, [{1,1}], 0)),
     ?_assertEqual(1, road:days(2, [{0,2}], 0)),
     ?_assertEqual(-1, road:days(2, [{1,2}], 0)),
     ?_assertEqual(-1, road:days(10, [{1,5},{6,10}], 0)),
     ?_assertEqual(2, road:days(100, [{1,10},{20,100}], 10)),

     %% The following test cases that I missed were found by PropEr.
     ?_assertEqual(2, road:days(1000, [{1,361},{458,900},{1,225}], 100))
    ].

days_assignment_grading_test_() ->
    [
     ?_assertEqual(2, road:days(30,[{1,5},{11,27},{2,14},{18,28}],6)),
     ?_assertEqual(-1, road:days(30,[{1,5},{11,27},{2,14},{18,28}],1)),
     ?_assertEqual(0, road:days(1,[{1,1}],1)),
     ?_assertEqual(-1, road:days(1,[{1,1}],0)),
     ?_assertEqual(1, road:days(2,[{1,1}],1)),
     ?_assertEqual(-1, road:days(1,[{1,1},{1,1}],0)),
     ?_assertEqual(6, road:days(58,[{57,57},{6,42},{19,23},{41,42},{15,36},{46,53},{8,46},{2,14},{58,58},{57,58},{17,28},{16,35},{23,26},{20,32}],8)),
     ?_assertEqual(3, road:days(2573,[{119,1209},{482,1901},{1729,2463},{66,2150},{602,976},{1176,2323},{1875,2370},{1703,2463},{1912,2214},{887,1363},{1765,2242},{2278,2505},{1452,2111},{34,1782},{2189,2229},{1825,2546},{1247,1851},{2287,2328},{910,2067},{2347,2395},{1519,1672},{1253,1509},{1416,1750},{205,948},{1479,1812},{207,906},{660,840},{2263,2401},{2406,2458},{2378,2448},{862,2122},{2439,2545},{1064,1680},{702,761},{2055,2275},{1784,2405},{1170,2446},{1072,1598},{1455,2100},{2507,2531},{1094,1608},{2184,2270},{1257,2519},{2058,2566},{360,1368},{2015,2221},{1846,2046},{355,2233},{2403,2436},{1407,1985},{608,1385},{781,1377},{543,1341},{294,1454},{82,1366},{515,1504},{700,2144},{361,2231},{56,281},{770,843},{781,2367},{1726,1783},{2092,2241},{740,2321},{1579,2408},{706,2490},{239,2215},{2487,2512},{978,1756},{1422,1470},{244,2280},{1139,1214},{1227,1791},{1510,2266},{1451,2011},{2321,2444},{265,1722},{2195,2412},{2394,2465},{1251,2184},{2548,2561},{2333,2509}],349))
    ].

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

is_schedulable_test_() ->
    [
     ?_assertEqual(true, is_schedulable(1, [], 0, 1)),
     ?_assertEqual(false, is_schedulable(1, [], 1, 1)),
     ?_assertEqual(false, is_schedulable(1, [{0,1}], 0, 1)),
     ?_assertEqual(true, is_schedulable(1, [{1,1}], 0, 1)),
     ?_assertEqual(true, is_schedulable(1, [{0,1}], 0, 2)),
     ?_assertEqual(true, is_schedulable(30, [], 0, 30)),
     ?_assertEqual(false, is_schedulable(31, [], 0, 30)),
     ?_assertEqual(true, is_schedulable(20, [{1,10}], 0, 30)),
     ?_assertEqual(false, is_schedulable(21, [{1,10}], 0, 30))
    ].

%%--------------------------------------------------------------------
%% Property-Based Tests
%%--------------------------------------------------------------------

prop_days_alt_verifies_correctly() ->
    ?FORALL(Intervals, list(interval()), verify_result(1000, Intervals, 100, days(1000, Intervals, 100))).

prop_days_gives_same_result_as_days_alt() ->
    ?FORALL(Intervals, list(interval()), days(1000, Intervals, 100) =:= days_alt(1000, Intervals, 100)).

prop_add_intervals_is_sorted_with_no_overlap() ->
    ?FORALL(Intervals, list(interval()), is_sorted_with_no_overlaps(add_intervals(Intervals))).

interval() ->
    ?LET(Lower, range(1, 900), {Lower, Lower + rand:uniform(500)}).

%% @doc
%% Verifies that the result of days/3 is correct.
-spec verify_result(pos_integer(), [interval()], non_neg_integer(), integer()) -> boolean().
verify_result(Length, Intervals, Goal, Result) when Result =:= -1 ->
    longest_interval(invert_intervals(add_n_intervals(Length, Intervals), 0, Length)) > Goal;
verify_result(Length, Intervals, Goal, Result) when Result =:= 0 ->
    longest_interval(invert_intervals(add_n_intervals(Result, Intervals), 0, Length)) =< Goal;
verify_result(Length, Intervals, Goal, Result) when Result > 0 ->
    longest_interval(invert_intervals(add_n_intervals(Result, Intervals), 0, Length)) =< Goal andalso
        longest_interval(invert_intervals(add_n_intervals(Result - 1, Intervals), 0, Length)) > Goal.

%% @doc
%% An alternate implementation of days/3 that is slower but uses a simpler algorithm.
-spec days_alt(pos_integer(), [interval()], non_neg_integer()) -> integer().
days_alt(Length, Intervals, Goal) ->
    days_alt(Length, Intervals, Goal, 0, []).

days_alt(Length, [], Goal, Day, IntervalsSoFar) ->
    case longest_interval(invert_intervals(IntervalsSoFar, 0, Length)) =< Goal of
        true -> Day;
        false -> -1
    end;
days_alt(Length, [Interval|Intervals], Goal, Day, IntervalsSoFar) ->
    case longest_interval(invert_intervals(IntervalsSoFar, 0, Length)) =< Goal of
        true -> Day;
        false -> days_alt(Length, Intervals, Goal, Day + 1, add_interval(Interval, IntervalsSoFar))
    end.

%% @doc
%% Verifies that a list of intervals is sorted by lower limit, and that there
%% are no overlapping intervals.
-spec is_sorted_with_no_overlaps([interval()]) -> boolean().
is_sorted_with_no_overlaps([]) -> true;
is_sorted_with_no_overlaps([{_L,_H}]) -> true;
is_sorted_with_no_overlaps([{L1,H1},{L2,H2}|Intervals]) ->
    L1 < L2 andalso H1 < L2 andalso is_sorted_with_no_overlaps([{L2,H2}|Intervals]).

%% @doc
%% Inverts the intervals in the list Intervals, given that Start and End are the
%% minimum and maximum values, respectively, of any intervals.
%%
%% Example: invert_intervals([{1,2},{4,8}], 1, 10) = [{2,4},{8,10}]
-spec invert_intervals([interval()], non_neg_integer(), pos_integer()) -> [interval()].
invert_intervals(Intervals, Start, End) -> invert_intervals(Intervals, Start, End, []).

%-spec invert_intervals([interval()], non_neg_integer(), pos_integer(), [interval()]) -> [interval()].
invert_intervals([], Start, End, Result) when Start =< End ->
    Result ++ [{Start, End}];
invert_intervals([], Start, End, Result) when Start > End ->
    Result;
invert_intervals([{L,H}|Intervals], Start, End, Result) when Start < L ->
    invert_intervals(Intervals, H, End, Result ++ [{Start, L}]);
invert_intervals([{L,H}|Intervals], Start, End, Result) when Start >= L ->
    invert_intervals(Intervals, H, End, Result).

%% Gives the length of the longest interval in the list Intervals.
-spec longest_interval([interval()]) -> non_neg_integer().
longest_interval(Intervals) -> longest_interval(Intervals, 0).

-spec longest_interval([interval()], non_neg_integer()) -> non_neg_integer().
longest_interval([], Result) -> Result;
longest_interval([{L,H}|Intervals], Result) when H - L > Result ->
    longest_interval(Intervals, H - L);
longest_interval([{L,H}|Intervals], Result) when H - L =< Result ->
    longest_interval(Intervals, Result).

%% @doc
%% Calls add_interval/2 on the the first N intervals in the list Intervals.
-spec add_n_intervals(non_neg_integer(), [interval()]) -> [interval()].
add_n_intervals(N, Intervals) ->
    add_intervals(lists:sublist(Intervals, N)).

%% @doc
%% Calls add_interval/2 on all intervals in the list Intervals.
-spec add_intervals([interval()]) -> [interval()].
add_intervals(Intervals) -> add_intervals(Intervals, []).

-spec add_intervals([interval()], [interval()]) -> [interval()].
add_intervals([], IntervalsSoFar) -> IntervalsSoFar;
add_intervals([Interval|Intervals], IntervalsSoFar) ->
    add_intervals(Intervals, add_interval(Interval, IntervalsSoFar)).

%%--------------------------------------------------------------------
%% Functions for manual performance testing
%%--------------------------------------------------------------------
%% Example of use:
%% Intervals = road:generate_random_intervals(1000000, 1000000000, 100000).
%% road:benchmark(days, [1000000000, Intervals, 100000], 10).
%% % Gives approximately 8000 ms on my machine
%%--------------------------------------------------------------------

%% @doc
%% Generates N random intervals with lower bound between 1 and Max - 1, and a
%% length between 1 and MaxLength.
-spec generate_random_intervals(non_neg_integer(), pos_integer(), pos_integer()) -> [interval()].
generate_random_intervals(N, Max, MaxLength) ->
    generate_random_intervals(N, Max, MaxLength, []).

-spec generate_random_intervals(non_neg_integer(), pos_integer(), pos_integer(), [interval()]) -> [interval()].
generate_random_intervals(0, _Max, _MaxLength, Intervals) -> Intervals;
generate_random_intervals(N, Max, MaxLength, Intervals) ->
    generate_random_intervals(N - 1, Max, MaxLength, [generate_random_interval(Max, MaxLength)|Intervals]).

-spec generate_random_interval(pos_integer(), pos_integer()) -> interval().
generate_random_interval(Max, MaxLength) ->
    L = rand:uniform(Max - 1),
    H = L + rand:uniform(MaxLength),
    case H > Max of
        true ->
            {L, Max};
        false ->
            {L, H}
    end.

%% @doc
%% Runs a function a number of times and returns the average run time in milliseconds.
-spec benchmark(fun(), [any()], pos_integer()) -> float().
benchmark(Fun, ArgList, NumRuns) ->
    Rs = [timer:tc(?MODULE, Fun, ArgList) || _ <- lists:seq(1, NumRuns)],
    lists:sum([T || {T, _} <- Rs]) / (1000 * length(Rs)).
