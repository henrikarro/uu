%%%-------------------------------------------------------------------
%%% @author Henrik Arro <henrik.arro@gmail.com>
%%%  [http://www.reallifedeveloper.com]
%%% @copyright 2017 Henrik Arro
%%% @doc Vector calculator server. A simple RPC server that evaluates
%%% vector expressions given in the language described below. After
%%% a connection has terminated, the server waits for a new connection.
%%% <p>
%%% <b>Language:</b><br/><br/>
%%% &lt;top&gt; ::= &lt;expr&gt;<br/>
%%% <br/>
%%% &lt;expr&gt; ::= &lt;vector&gt;
%%% | {&lt;vector-op&gt;, &lt;expr&gt;, &lt;expr&gt;}
%%% | {&lt;scalar-op&gt;, &lt;int-expr&gt;, &lt;expr&gt;}<br/>
%%% <br/>
%%% &lt;vector&gt; ::= [&lt;integer&gt;, ...]<br/>
%%% <br/>
%%% &lt;vector-op&gt; ::= ’add’ | ’sub’ | ’dot’<br/>
%%% <br/>
%%% &lt;scalar-op&gt; ::= ’mul’ | ’div’<br/>
%%% <br/>
%%% &lt;int-expr&gt; ::= &lt;integer&gt; | {&lt;norm&gt;, &lt;expr&gt;}<br/>
%%% <br/>
%%% &lt;norm&gt; ::= ’norm_one’ | ’norm_inf’
%%% </p>
%%% @end
%%%-------------------------------------------------------------------

-module(vector_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-export([eval_expr/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

% The state of the server
-record(state, {port :: char(), lsock :: port()}).
-type state() :: #state{}.

-type expr() :: vector() | {vector_op(), expr(), expr()} | {scalar_op(), int_expr(), expr()}.
-type vector() :: [integer(), ...].
-type vector_op() :: 'add' | 'sub' | 'dot'.
-type scalar_op() :: 'mul' | 'div'.
-type int_expr() :: integer() | {norm(), expr()}.
-type norm() :: 'norm_one' | 'norm_inf'.

-export_type([expr/0, vector/0, vector_op/0, scalar_op/0, int_expr/0, norm/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%--------------------------------------------------------------------
-spec start_link(integer()) -> {ok, pid()}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Calls `start_link(Port)' using the default port.
-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([char(), ...]) -> {ok, state()}.
init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    gen_server:cast(self(), accept),
    {ok, #state{port = Port, lsock = LSock}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-type cast_request() :: accept | stop.
-type cast_result() :: {noreply, state()} | {stop, normal, state()}.
-spec handle_cast(cast_request(), state()) -> cast_result().
handle_cast(accept, State = #state{lsock = LSock}) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

-type info_request() :: {tcp, port(), iolist()} | {tcp_closed, port()}.
-type info_result() :: {noreply, state()}.
-spec handle_info(info_request(), state()) -> info_result().
handle_info({tcp, Socket, RawData}, State) ->
    ok = try
        ResultTerm = handle_input(RawData),
        gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [ResultTerm]))
    catch
	{eval_error, _ErrorMessage} ->
            gen_tcp:send(Socket, io_lib:fwrite("Res: ~s~n", ["error"]))
    end,
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    gen_server:cast(self(), accept),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec handle_input(iolist()) -> vector().
handle_input(String) ->
    Term = convert_string_to_term(String),
    eval_expr(Term).

-spec convert_string_to_term(iolist()) -> term().
convert_string_to_term(RawData) ->
    String = lists:flatten(io_lib:format("~s.", [RawData])),
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    Term;
                {error, {_ErrorLocation, _Module, ErrorDescriptor}} ->
                    ErrorMessage = lists:concat(erl_parse:format_error(ErrorDescriptor)),
                    throw({eval_error, ErrorMessage})
            end;
        {error, {_ErrorLocation, _Module, ErrorDescriptor}, _ErrorLocation} ->
            ErrorMessage = lists:concat(erl_scan:format_error(ErrorDescriptor)),
            throw({eval_error, ErrorMessage})
    end.

-spec eval_expr(expr()) -> vector().
eval_expr(E) -> eval_expr(E, 0).
% If the following spec is not commented out, Dialyzer gives a strange warning
%-spec eval_expr(expr(), non_neg_integer()) -> vector().
eval_expr(_E, Depth) when Depth > 101 -> throw({eval_error, "Expression too deeply nested"});
eval_expr(E, _Depth) when is_list(E) -> eval_vector(E);
eval_expr({Op, E1, E2}, Depth) when Op =:= add orelse Op =:= sub orelse Op =:= dot ->
    T1 = eval_expr(E1, Depth + 1),
    T2 = eval_expr(E2, Depth + 1),
    case Op of
        add -> vector_op(fun(X, Y) -> X + Y end, T1, T2);
        sub -> vector_op(fun(X, Y) -> X - Y end, T1, T2);
        dot -> vector_op(fun(X, Y) -> X * Y end, T1, T2)
    end;
eval_expr({Op, E1, E2}, Depth) when Op =:= mul orelse Op =:= 'div' ->
    T1 = eval_int_expr(E1, Depth + 1),
    T2 = eval_expr(E2, Depth + 1),
    try
        case Op of
            mul -> lists:map(fun(X) -> X * T1 end, T2);
            'div' -> lists:map(fun(X) -> X div T1 end, T2)
        end
    catch
        error:Term ->
            throw({eval_error, io_lib:fwrite("Error: ~w", [Term])})
    end;
eval_expr(Expr, _Depth) -> throw({eval_error, io_lib:format("Illegal expression: ~w", [Expr])}).

-spec eval_vector(vector()) -> vector().
eval_vector([]) -> throw({eval_error, "Empty vectors are not allowed"});
eval_vector([X]) when is_integer(X) -> [X];
eval_vector([X|Xs]) when is_integer(X) andalso length([X|Xs]) =< 100 ->
    [X|eval_vector(Xs)];
eval_vector([X|Xs]) when length([X|Xs]) > 100 ->
    throw({eval_error, "Length of vectors must be <= 100"});
eval_vector(_) ->
    throw({eval_error, "Vectors can only contain integers"}).

-spec eval_int_expr(int_expr(), non_neg_integer()) -> integer().
eval_int_expr(N, _Depth) when is_integer(N) -> N;
eval_int_expr({norm_one, E}, Depth) ->
    T = eval_expr(E, Depth + 1),
    lists:foldl(fun(X, Y) -> abs(X) + Y end, 0, T);
eval_int_expr({norm_inf, E}, Depth) ->
    T = eval_expr(E, Depth + 1),
    lists:foldl(fun(X, Y) -> max(abs(X), Y) end, 0, T);
eval_int_expr(Expr, _Depth) ->
    throw({eval_error, io_lib:format("Illegal integer expression: ~w", [Expr])}).

-type int_function() :: fun((integer(), integer()) -> integer()).
-spec vector_op(int_function(), vector(), vector()) -> vector().
vector_op(F, L1, L2) ->
    case length(L1) =:= length(L2) of
        true -> lists:zipwith(F, L1, L2);
        false -> throw({eval_error, "Vectors must be same length"})
    end.


%%--------------------------------------------------------------------
%% EUnit Test Cases
%%--------------------------------------------------------------------

convert_string_to_term_test_() ->
    [?_assertEqual(foo, convert_string_to_term("foo")),
     ?_assertEqual([1,2,3], convert_string_to_term("[1, 2, 3]")),
     ?_assertEqual({add, [1,2,3], [2,3,4]},
                   convert_string_to_term("{add, [1, 2, 3], [2, 3, 4]}")),
     ?_assertException(throw, {eval_error, "syntax error before: bar"},
                   convert_string_to_term("foo bar"))
    ].

vector_op_test_() ->
    [?_assertEqual([3,5,7], vector_op(fun(X,Y)->X+Y end, [1,2,3], [2,3,4]))
    ].

eval_expr_test_cases_from_assignment_description_test_() ->
    [
     ?_assertEqual([1,2,3,4,5], eval_expr([1,2,3,4,5])),
     ?_assertEqual([42,42,42], eval_expr({'dot', [6,6,6], [7,7,7]})),
     ?_assertEqual([42,-42,42], eval_expr({'mul', {'norm_one', [1,-1,2,-2]}, [7,-7,7]}))
    ].

eval_expr_test_() ->
    [
     ?_assertEqual([5,7,9], eval_expr({add, [1,2,3], [4,5,6]})),
     ?_assertEqual([10 * 2 + 3], eval_expr(create_nested_expr(10, add, [1], [2]))),
     ?_assertEqual([1,2,3], eval_expr({sub, [5,7,9],[4,5,6]})),
     ?_assertEqual([10 * -2 - 1], eval_expr(create_nested_expr(10, sub, [1], [2]))),
     ?_assertEqual([4,10,18], eval_expr({dot, [1,2,3], [4,5,6]})),
     ?_assertEqual([round(math:pow(2, 10 + 1))], eval_expr(create_nested_expr(10, dot, [1], [2]))),
     ?_assertEqual([8,10,12], eval_expr({mul, 2, [4,5,6]})),
     ?_assertEqual([24,30,36], eval_expr({mul, {norm_one, [-1,-3,2]}, [4,5,6]})),
     ?_assertEqual([92,115,138], eval_expr({mul, {norm_one, create_nested_expr(10, add, [1], [2])}, [4,5,6]})),
     ?_assertEqual([12,15,18], eval_expr({mul, {norm_inf, [-1,-3,2]}, [4,5,6]})),
     ?_assertEqual([92,115,138], eval_expr({mul, {norm_inf, create_nested_expr(10, add, [1], [2])}, [4,5,6]})),
     ?_assertEqual([2,2,3], eval_expr({'div', 2, [4,5,6]})),
     ?_assertEqual([0,0,1], eval_expr({'div', {norm_one, [-1,-3,2]}, [4,5,6]})),
     ?_assertEqual([1,1,2], eval_expr({'div', {norm_inf, [-1,-3,2]}, [4,5,6]})),
     % Check that vectors of length 100 are OK
     ?_assertEqual([2*X || X <- lists:seq(1,100)], eval_expr({add, lists:seq(1,100), lists:seq(1,100)})),
     % Check that nested expressions 100 deep are OK
     ?_assertEqual([100 * 2 + 3], eval_expr(create_nested_expr(100, add, [1], [2]))),
     ?_assertEqual([98 * 2 + 3], eval_expr({mul, {norm_inf, create_nested_expr(98, add, [1], [2])}, [1]}))
    ].

eval_expr_error_test_() ->
    [
     fun() -> assert_eval_expr_error({'div', 0, [1,2,3,4,5]}, eval_error, "Error: badarith") end,
     fun() -> assert_eval_expr_error({'div', 0, [0]}, eval_error, "Error: badarith") end,
     fun() -> assert_eval_expr_error({'div', 0, []}, eval_error, "Empty vectors are not allowed") end,
     fun() -> assert_eval_expr_error({add, [], []}, eval_error, "Empty vectors are not allowed") end,
     fun() -> assert_eval_expr_error({add, lists:seq(1,101), lists:seq(1,101)}, eval_error, "Length of vectors must be <= 100") end,
     fun() -> assert_eval_expr_error(create_nested_expr(101, add, [1], [2]), eval_error, "Expression too deeply nested") end,
     fun() -> assert_eval_expr_error({mul, {norm_inf, create_nested_expr(99, add, [1], [2])}, [1]}, eval_error, "Expression too deeply nested") end,
     fun() -> assert_eval_expr_error({mul, {foo, [1,2]}, [3,4]}, eval_error, "Illegal integer expression: {foo,[1,2]}") end,
     fun() -> assert_eval_expr_error({add, [1,2], [1,2,3]}, eval_error, "Vectors must be same length") end,
     fun() -> assert_eval_expr_error({sub, [1,2], [1,2,3]}, eval_error, "Vectors must be same length") end,
     fun() -> assert_eval_expr_error({dot, [1,2], [1,2,3]}, eval_error, "Vectors must be same length") end
    ].

%% Verifies that eval_expr(Expr) fails with exception {Error, ExpectedErrorMessage}.
%%
%% This function is necessary in order to convert the error message from iolist to string.
assert_eval_expr_error(Expr, Error, ExpectedErrorMessage) ->
    try
        _V = eval_expr(Expr),
        ?assert(false)
    catch
        {Error, ErrorMessage} ->
            ?assertEqual(ExpectedErrorMessage, lists:flatten(ErrorMessage))
    end.

create_nested_expr(0, Op, T1, T2) -> {Op, T1, T2};
create_nested_expr(N, Op, T1, T2) -> {Op, create_nested_expr(N-1, Op, T1, T2), T2}.
