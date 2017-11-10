-module(calc_pi).

-export([calc/2, monte_carlo/2]).

%% @doc
%% Runs a Monte Carlo simulation to approximate Pi, using a total of
%% N simulation steps spread between Schedulers * 4 number of processes.
-spec calc(pos_integer(), pos_integer()) -> float().
calc(N, Schedulers) ->
    NumWorkers = Schedulers * 4,
    spawn_workers(round(N / NumWorkers), NumWorkers),
    C = receive_replies(NumWorkers, 0),
    4 * C / N.

%% @doc
%% Creates NumWorkers new processes that each calls monte_carlo(N, 0)
%% and sends a message back to the creating process with the result.
-spec spawn_workers(pos_integer(), non_neg_integer()) -> ok.
spawn_workers(_N, 0) -> ok;
spawn_workers(N, NumWorkers) ->
    Pid = self(),
    spawn(fun() -> C = monte_carlo(N, 0), Pid ! {monte_carlo, C} end),
    spawn_workers(N, NumWorkers - 1).

%% @doc
%% Waits for replies from NumWorkers processes and returns the sum
%% of the monte_carlo/2 results from each process (plus C).
-spec receive_replies(non_neg_integer(), integer()) -> integer().
receive_replies(0, C) -> C;
receive_replies(NumWorkers, C) ->
    receive
        {monte_carlo, NewC} ->
            receive_replies(NumWorkers - 1, C + NewC)
    end.

%% @doc
%% Runs a Monte Carlo simulation with N random points with coordinates
%% between 0 and 1 and returns the number of points that are within the
%% unit circle.
-spec monte_carlo(non_neg_integer(), integer()) -> integer().
monte_carlo(0, C) -> C;
monte_carlo(N, C) ->
    {X, Y} = random_point(),
    case X * X + Y * Y < 1 of
        true ->
            monte_carlo(N - 1, C + 1);
        false ->
            monte_carlo(N - 1, C)
    end.

%% @doc
%% Creates a point {X, Y} with random X and Y coordinates between
%% zero (inclusive) and one (exclusive).
-spec random_point() -> {float(), float()}.
random_point() ->
    X = rand:uniform(),
    Y = rand:uniform(),
    {X, Y}.
