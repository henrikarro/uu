-module(bughunt).

-export([test/1, run_tests/0, run_specific_tests/1]).

%-export([prop_sub_mul/1, prop_mul_norm_inf/1, prop_with_type_as_generator/1,
%         prop_long_or_empty_vectors/1, prop_deep_expr/1, prop_deep_int_expr/1]).

-include_lib("proper/include/proper.hrl").

-type expr() :: vector_server:expr().
-type vector() :: vector_server:vector().

-type eval_result() :: vector() | error.
-type evaluator() :: fun((expr()) -> eval_result()).

-type test_result() :: correct | {expr(), eval_result(), eval_result(), string()}.
-type property_info() :: {proper:outer_test(), pos_integer(), string()}.

%% @doc
%% Runs property-based tests for evaluator implementation number N.
-spec test(pos_integer()) -> test_result().
test(Id) ->
    Evaluator = vectors:vector(Id),
    Properties = [
                  {bughunt:prop_sub_mul(Evaluator), 1, "Does not handle {sub,[0],{mul,0,[0]}}"},
                  {bughunt:prop_mul_norm_inf(Evaluator), 1, "Does not handle {mul,{norm_inf,[0]},[1]}"},
                  {bughunt:prop_with_type_as_generator(Evaluator), 500, "Discovered using type as generator"},
                  {bughunt:prop_long_or_empty_vectors(Evaluator), 500, "Does not handle long or empty vectors"},
                  {bughunt:prop_deep_expr(Evaluator), 500, "Does not handle deeply nested expr"},
                  {bughunt:prop_deep_int_expr(Evaluator), 500, "Does not handle deeply nested int_expr"}
                 ],
    test_with_properties(Evaluator, Properties).

-spec test_with_properties(evaluator(), [property_info()]) -> test_result().
test_with_properties(_Evaluator, []) -> correct;
test_with_properties(Evaluator, [{Property, NumTests, Comment}|Properties]) ->
    Result = proper:quickcheck(Property, [{numtests, NumTests}, quiet]),
    case Result of
        true ->
            test_with_properties(Evaluator, Properties);
        false ->
            test_result(Evaluator, Comment)
    end.

test_result(Evaluator, Message) ->
            FailedExpr = hd(proper:counterexample()),
            {FailedExpr, eval_expr(FailedExpr), Evaluator(FailedExpr), Message}.

%%--------------------------------------------------------------------
%% Properties
%%--------------------------------------------------------------------

prop_sub_mul(Evaluator) ->
    ?FORALL(Expr, {sub,[0],{mul,0,[0]}}, eval_expr(Expr) =:= Evaluator(Expr)).

prop_mul_norm_inf(Evaluator) ->
    ?FORALL(Expr, {mul,{norm_inf,[0]},[8,1]}, eval_expr(Expr) =:= Evaluator(Expr)).

prop_with_type_as_generator(Evaluator) ->
    ?FORALL(Expr, vector_server:expr(), eval_expr(Expr) =:= Evaluator(Expr)).

prop_long_or_empty_vectors(Evaluator) ->
        ?FORALL(Expr, my_expr_long_or_empty_vectors(), eval_expr(Expr) =:= Evaluator(Expr)).

prop_deep_expr(Evaluator) ->
    ?FORALL(Expr, my_deep_expr(), eval_expr(Expr) =:= Evaluator(Expr)).

prop_deep_int_expr(Evaluator) ->
    ?FORALL(Expr, {mul, my_deep_int_expr(), my_vector()}, eval_expr(Expr) =:= Evaluator(Expr)).

%% Translates exception error handling to the error handling used in the vectors module.
eval_expr(Expr) ->
    try
        vector_server:eval_expr(Expr)
    catch
        {eval_error, _ErrorMessage} ->
            error
    end.

%%--------------------------------------------------------------------
%% Generators for expressions with long or empty vectors
%%--------------------------------------------------------------------

my_expr_long_or_empty_vectors() -> ?SIZED(N, my_expr_long_or_empty_vectors(N)).
my_expr_long_or_empty_vectors(0) -> my_long_vector();
my_expr_long_or_empty_vectors(N) ->
    union([
           [],
           my_long_vector(),
           {my_vector_op(), my_expr_long_or_empty_vectors(N div 2), my_expr_long_or_empty_vectors(N div 2)},
           {my_scalar_op(), my_int_expr_long_or_empty_vectors(N div 2), my_expr_long_or_empty_vectors(N div 2)}
          ]).

my_long_vector() ->
    ?LET(N, range(98, 102), vector(N, integer())).

my_int_expr_long_or_empty_vectors(0) -> integer();
my_int_expr_long_or_empty_vectors(N) ->
    union([
           integer(),
           {my_norm(), my_expr_long_or_empty_vectors(N div 2)}
          ]).

%%--------------------------------------------------------------------
%% Generators for deeply nested expressions (with no int_expr)
%%--------------------------------------------------------------------

my_deep_expr() ->
    ?LET(N, range(98, 102), my_deep_expr(N)).

my_deep_expr(0) -> my_vector();
my_deep_expr(N) ->
    union([
           {my_vector_op(), my_deep_expr(N - 1), my_deep_expr(0)}
          ]).

%%--------------------------------------------------------------------
%% Generators for deeply nested integer expressions
%%--------------------------------------------------------------------

my_deep_int_expr() ->
    ?LET(N, range(98, 102), my_deep_int_expr(N)).

my_deep_int_expr(0) -> integer();
my_deep_int_expr(N) ->
    union([
           {my_norm(), my_deep_expr(N - 1)}
          ]).

%%--------------------------------------------------------------------
%% Generators for normal vectors
%%--------------------------------------------------------------------

my_vector() ->
    weighted_union([
                    {1000, vector(3, integer())},
                    {1, vector(4, integer())},
                    {1, vector(99, integer())},
                    {1, vector(100, integer())},
                    {1, vector(101, integer())}
                   ]).

my_vector_op() -> union(['add', 'sub', 'dot']).

my_scalar_op() -> union(['mul', 'div']).

my_norm() -> union(['norm_one', 'norm_inf']).

%%--------------------------------------------------------------------
%% Helper functions for running tests
%%--------------------------------------------------------------------

%% @doc
%% Runs all tests for the 50 evaluator implementations.
run_tests() -> run_tests(1).

run_tests(N) when N > 50 -> ok;
run_tests(N) ->
    run_test(N),
    run_tests(N + 1).

%% @doc
%% Runs all tests for evaluator implementation number N.
run_test(N) ->
    try
        case test(N) of
            correct ->
                io:format("~w: correct~n", [N]);
            {_FailedExpr, _ExpectedOutput, _ActualOutput, Comment} ->
                io:format("~w: incorrect: ~s~n", [N, Comment])
        end
    catch Exception ->
            io:format("~w: incorrect: exception ~w~n", [N, Exception]);
          error:Exception ->
            io:format("~w: incorrect: error ~w~n", [N, Exception])
    end.

%% @doc
%% Runs all tests for the evaluator implementations number in the list
run_specific_tests([]) -> ok;
run_specific_tests([N|Ns]) ->
    run_test(N),
    run_specific_tests(Ns).
