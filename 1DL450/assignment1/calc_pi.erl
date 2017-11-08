-module(calc_pi).

-export([calc/2, monte_carlo/2]).

calc(N, Schedulers) ->
    spawn_workers(round(N / Schedulers), Schedulers),
    C = receive_replies(Schedulers, 0),
    4 * C / N.

spawn_workers(_N, 0) -> ok;
spawn_workers(N, NumWorkers) ->
    Pid = self(),
    spawn(fun() -> C = monte_carlo(N, 0), Pid ! {monte_carlo, C} end),
    spawn_workers(N, NumWorkers - 1).

receive_replies(0, C) -> C;
receive_replies(NumWorkers, C) ->
    receive
        {monte_carlo, NewC} ->
            receive_replies(NumWorkers - 1, C + NewC)
    end.

monte_carlo(0, C) -> C;
monte_carlo(N, C) ->
    {X, Y} = random_point(),
    case X * X + Y * Y < 1 of
        true ->
            monte_carlo(N - 1, C + 1);
        false ->
            monte_carlo(N - 1, C)
    end.

random_point() ->
    X = rand:uniform(),
    Y = rand:uniform(),
    {X, Y}.
