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
%%% &lt;norm&gt; ::= ’norm one’ | ’norm inf’
%%% </p>
%%% @end
%%%-------------------------------------------------------------------

-module(vector_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock}).

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

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    gen_server:cast(self(), accept),
    {ok, #state{port = Port, lsock = LSock}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(accept, State = #state{lsock = LSock}) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    io:format("Incoming: ~s", [RawData]),
    try
        ResultTerm = handle_input(RawData),
        gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [ResultTerm]))
    catch
        {syntax_error, _ErrorMessage} ->
            gen_tcp:send(Socket, io_lib:fwrite("Res: ~s~n", ["error"]))
    end,
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    io:format("Socket closed~n"),
    gen_server:cast(self(), accept),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Private functions
%%%===================================================================

handle_input(String) ->
    Term = convert_string_to_term(String),
    eval(Term).

convert_string_to_term(RawData) ->
    String = lists:flatten(io_lib:format("~s.", [RawData])),
    case erl_scan:string(String) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    Term;
                {error, {_ErrorLocation, _Module, ErrorDescriptor}} ->
                    ErrorMessage = lists:concat(erl_parse:format_error(ErrorDescriptor)),
                    throw({syntax_error, ErrorMessage})
            end;
        {error, {_ErrorLocation, _Module, ErrorDescriptor}, _ErrorLocation} ->
            ErrorMessage = lists:concat(erl_scan:format_error(ErrorDescriptor)),
            throw({syntax_error, ErrorMessage})
    end.

eval(E) when is_list(E) -> eval_vector(E);
eval({Op, E1, E2}) when Op =:= add orelse Op =:= sub orelse Op =:= dot ->
    T1 = eval(E1),
    T2 = eval(E2),
    case Op of
        add -> vector_op(fun(X, Y) -> X + Y end, T1, T2);
        sub -> vector_op(fun(X, Y) -> X - Y end, T1, T2);
        dot -> vector_op(fun(X, Y) -> X * Y end, T1, T2)
    end;
eval({Op, E1, E2}) when Op =:= mul orelse Op =:= 'div' ->
    T1 = eval_int_expr(E1),
    T2 = eval(E2),
    try
        case Op of
            mul -> lists:map(fun(X) -> X * T1 end, T2);
            'div' -> lists:map(fun(X) -> X div T1 end, T2)
        end
    catch
        error:Term ->
            throw({runtime_error, io_lib:format("Error: ~w", [Term])})
    end;
eval(Expr) -> throw({syntax_error, io_lib:format("Illegal expression: ~w", [Expr])}).

eval_vector([]) -> throw({syntax_error, "Empty vectors are not allowed"});
eval_vector([X]) when is_integer(X) -> [X];
eval_vector([X|Xs]) when is_integer(X) andalso length([X|Xs]) =< 100 ->
    [X|eval_vector(Xs)];
eval_vector([X|Xs]) when length([X|Xs]) > 100 ->
    throw({syntax_error, "Length of vectors must be <= 100"});
eval_vector(_) ->
    throw({syntax_error, "Vectors can only contain integers"}).

eval_int_expr(N) when is_integer(N) -> N;
eval_int_expr({norm_one, E}) ->
    T = eval(E),
    lists:foldl(fun(X, Y) -> abs(X) + Y end, 0, T);
eval_int_expr({norm_inf, E}) ->
    T = eval(E),
    lists:foldl(fun(X, Y) -> max(abs(X), Y) end, 0, T);
eval_int_expr(Expr) ->
    throw({syntax_error, io_lib:format("Illegal integer expression: ~w", [Expr])}).

vector_op(F, L1, L2) ->
    case length(L1) =:= length(L2) of
        true -> lists:zipwith(F, L1, L2);
        false -> throw({syntax_error, "Vectors must be same lentgh"})
    end.


%%--------------------------------------------------------------------
%% EUnit Test Cases
%%--------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

convert_string_to_term_test_() ->
    [?_assertEqual(foo, convert_string_to_term("foo")),
     ?_assertEqual([1,2,3], convert_string_to_term("[1, 2, 3]")),
     ?_assertEqual({add, [1,2,3], [2,3,4]},
                   convert_string_to_term("{add, [1, 2, 3], [2, 3, 4]}")),
     ?_assertException(throw, {syntax_error, "syntax error before: bar"},
                   convert_string_to_term("foo bar"))
    ].

vector_op_test_() ->
    [?_assertEqual([3,5,7], vector_op(fun(X,Y)->X+Y end, [1,2,3], [2,3,4]))
    ].
