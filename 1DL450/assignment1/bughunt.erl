%%%===================================================================
%%% @author Henrik Arro <henrik.arro@gmail.com>
%%%  [http://reallifedeveloper.com]
%%%
%%% @copyright 2017 Henrik Arro
%%%
%%% @doc Runs property-based tests against 50 different implementations
%%% of evaluators for the vector expression language.
%%%
%%% All properties compare the result of evaluating expressions using
%%% the evaluator under test with the result of evaluating the
%%% expressoins using {@link vector_server:eval_expr/1}. The
%%% properties differ in how the expressions are generated.
%%%
%%% @see vector_server:expr()
%%% @end
%%%===================================================================

-module(bughunt).

-export([test/1, run_tests/0, run_specific_tests/1]).

%% Note: I think the properties should be exported, but if the following two
%% export lines are not commented, I get the following error when running
%% proper:quickcheck:
%%
%%{error,
%%    {typeserver,
%%        {cant_load_code,bughunt,
%%            {no_abstract_code,
%%                {cant_compile_source_file,
%%                    [{"bughunt.erl",
%%                      [{25,erl_lint,{undefined_function,{prop_deep_expr,1}}},
%%                       {25,erl_lint,
%%                        {undefined_function,{prop_deep_int_expr,1}}},
%%                       {25,erl_lint,{undefined_function,{prop_expr,2}}},
%%                       {25,erl_lint,
%%                        {undefined_function,{prop_long_or_empty_vectors,1}}},
%%                       {25,erl_lint,
%%                        {undefined_function,
%%                            {prop_with_type_as_generator,1}}}]}]}}}}}
%%
%% The only solution I have found is to comment out the exports, and then use
%% c(bughunt, [export_all]) in the shell.
%%
-export([prop_expr/2, prop_with_type_as_generator/1, prop_long_or_empty_vectors/1,
         prop_deep_expr/1, prop_deep_int_expr/1]).

-include_lib("proper/include/proper.hrl").

-type expr() :: vector_server:expr().
-type vector() :: vector_server:vector().
-type scalar_op() :: vector_server:scalar_op().

-type eval_result() :: vector() | error.
-type evaluator() :: fun((expr()) -> eval_result()).

-type test_result() :: correct | failed_test_result().
-type failed_test_result() :: {expr(), eval_result(), eval_result(), string()}.

-type property_info() :: {proper:outer_test(), pos_integer(), string()}.

%% @doc
%% Runs property-based tests for evaluator implementation number N.
-spec test(pos_integer()) -> test_result().
test(Id) ->
    Evaluator = vectors:vector(Id),
    Properties = [
                  % First a few specific expressions that we have found during testing:
                  {bughunt:prop_expr(Evaluator, {sub,[0],{mul,0,[0]}}), 1, "Does not handle {sub,[0],{mul,0,[0]}}"},
                  {bughunt:prop_expr(Evaluator, {mul,{norm_inf,[0]},[1]}), 1, "Does not handle {mul,{norm_inf,[0]},[1]}"},
                  {bughunt:prop_expr(Evaluator, {mul,{norm_one,[]},[1]}), 1, "Does not handle {mul,{norm_one,[]},[1]}"},
                  {bughunt:prop_expr(Evaluator, {mul,{norm_one,lists:seq(1,101)},[1]}), 1, "Does not handle {mul,{norm_one,[0..101]},[1]}"},
                  {bughunt:prop_expr(Evaluator, {mul,{norm_one,[-1]},[1]}), 1, "Does not handle {mul,{norm_one,[-1]},[1]}"},
                  {bughunt:prop_expr(Evaluator, create_nested_scalar_expr(100, mul, 1, [1])), 1, "Does not handle {mul,1,{mul,1,{mul,1{...}}}} 100 deep"},

                  % Now the 'real' properties:
                  {bughunt:prop_with_type_as_generator(Evaluator), 400, "Discovered using type as generator"},
                  {bughunt:prop_long_or_empty_vectors(Evaluator), 200, "Does not handle long or empty vectors"},
                  {bughunt:prop_deep_expr(Evaluator), 200, "Does not handle deeply nested expr"},
                  {bughunt:prop_deep_int_expr(Evaluator), 200, "Does not handle deeply nested int_expr"}
                 ],
    test_with_properties(Evaluator, Properties).

%% @doc
%% Creates a nested scalar expression {Op,N,{Op,N,{...{Op,N,V}...}}}.
-spec create_nested_scalar_expr(non_neg_integer(), scalar_op(), integer(), vector()) -> expr().
create_nested_scalar_expr(0, Op, N, V) -> {Op, N, V};
create_nested_scalar_expr(Depth, Op, N, V) -> {Op, N, create_nested_scalar_expr(Depth - 1, Op, N, V)}.

%% @doc
%% Runs tests specified by the property info list using the given evaluator.
%% Each proprety info specifies the property to use, the  number of tests to run,
%% and the comment to use if a test fails. The result is either 'correct' if no
%% test failed, or a test result showing the expression that failed, the expected
%% and actual results, and the given comment.
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

%% @doc
%% Creates a test result for a failed test, containing the expression that caused
%% the failure, the expected and actual results, and a comment describing the
%% error.
%%
%% The expression that cause the failure is taken from PropEr, using the funcion
%% `proper:counterexample()'.
-spec test_result(evaluator(), string()) -> failed_test_result().
test_result(Evaluator, Comment) ->
            FailedExpr = hd(proper:counterexample()),
            {FailedExpr, eval_expr(FailedExpr), Evaluator(FailedExpr), Comment}.

%%%===================================================================
%% Properties
%%%===================================================================

%% @doc
%% Property that alwayas uses the given expression.
prop_expr(Evaluator, Expr) ->
    ?FORALL(E, gen_static_expr(Expr), eval_expr(E) =:= Evaluator(E)).

%% @doc
%% Property that uses random expressions based on the `expr' type.
prop_with_type_as_generator(Evaluator) ->
    ?FORALL(Expr, expr(), eval_expr(Expr) =:= Evaluator(Expr)).

%% @doc
%% Property that uses random expression with long or empty vectors.
prop_long_or_empty_vectors(Evaluator) ->
        ?FORALL(Expr, gen_expr_long_or_empty_vectors(), eval_expr(Expr) =:= Evaluator(Expr)).

%% @doc
%% Property that uses random deeply nested expressions.
prop_deep_expr(Evaluator) ->
    ?FORALL(Expr, gen_deep_expr(), eval_expr(Expr) =:= Evaluator(Expr)).

%% @doc
%% Property that ues random expressions containing deeploy nested integer expressions.
prop_deep_int_expr(Evaluator) ->
    ?FORALL(Expr, {mul, gen_deep_int_expr(), gen_vector()}, eval_expr(Expr) =:= Evaluator(Expr)).

%% @doc
%% Evaluates the given expression using the reference evaluator and translates exception
%% error handling to the error handling used in the vectors module.
-spec eval_expr(expr()) -> eval_result().
eval_expr(Expr) ->
    try
        vector_server:eval_expr(Expr)
    catch
        {eval_error, _ErrorMessage} ->
            error
    end.

%%%===================================================================
%%% Generators
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generator that always produces the given expression.
%% @end
% I'm sure there is a better way to tell PropEr to use some static value.
%%--------------------------------------------------------------------
gen_static_expr(Expr) ->
    union([Expr]).


%%--------------------------------------------------------------------
%% @doc
%% Generator that produces expressions with long or empty vectors.
%% @end
%%--------------------------------------------------------------------
gen_expr_long_or_empty_vectors() -> ?SIZED(N, gen_expr_long_or_empty_vectors(N)).

%% @doc
%% Generator that produces nested expressions with long or empty vectors.
%% The nesting depth is log2(N).
gen_expr_long_or_empty_vectors(0) -> gen_long_vector();
gen_expr_long_or_empty_vectors(N) ->
    union([
           [],
           gen_long_vector(),
           {gen_vector_op(), gen_expr_long_or_empty_vectors(N div 2), gen_expr_long_or_empty_vectors(N div 2)},
           {gen_scalar_op(), gen_int_expr_long_or_empty_vectors(N div 2), gen_expr_long_or_empty_vectors(N div 2)}
          ]).

%% @doc
%% Generator that produces vectors with between 98 and 102 integer elements.
gen_long_vector() ->
    ?LET(N, range(98, 102), vector(N, integer())).

%% @doc
%% Generator that produces an integer expression with a nested expression
%% with long or empty vectors.
gen_int_expr_long_or_empty_vectors(0) -> integer();
gen_int_expr_long_or_empty_vectors(N) ->
    union([
           integer(),
           {gen_norm_op(), gen_expr_long_or_empty_vectors(N div 2)}
          ]).

%%--------------------------------------------------------------------
%% @doc
%% Generator that produces deeply nested expressions (with no int_expr)
%% @end
%%--------------------------------------------------------------------
gen_deep_expr() ->
    ?LET(N, range(98, 102), gen_deep_expr(N)).

%% @doc
%% Generator that produces a nested expression with the given depth.
gen_deep_expr(0) -> gen_vector();
gen_deep_expr(N) ->
    union([
           {gen_vector_op(), gen_deep_expr(N - 1), gen_deep_expr(0)}
          ]).

%%--------------------------------------------------------------------
%% @doc
%% Generator that produces expressions that contain deeply nested
%% integer expressions.
%% @end
%%--------------------------------------------------------------------
gen_deep_int_expr() ->
    ?LET(N, range(98, 102), gen_deep_int_expr(N)).

%% @doc
%% Generator that produces an expression containing an nested integer
%% expression with the given depth.
gen_deep_int_expr(0) -> integer();
gen_deep_int_expr(N) ->
    union([
           {gen_norm_op(), gen_deep_expr(N - 1)}
          ]).

%%--------------------------------------------------------------------
%% Generators for normal vectors and operators
%%--------------------------------------------------------------------

%% @doc
%% Generator that produces 'normal' vectors, where the majority are
%% of length 3, some are of other lengths, both legal and illegal.
gen_vector() ->
    weighted_union([
                    {1, []},
                    {1000, vector(3, integer())},
                    {1, vector(4, integer())},
                    {1, vector(99, integer())},
                    {1, vector(100, integer())},
                    {1, vector(101, integer())}
                   ]).

%% @doc
%% Generator that produces a random {@link vector_server:vector_op()}.
gen_vector_op() -> union(['add', 'sub', 'dot']).

%% @doc
%% Generator that produces a random {@link vector_server:scalar_op()}.
gen_scalar_op() -> union(['mul', 'div']).

%% @doc
%% Generator that produces a random {@link vector_server:norm()}.
gen_norm_op() -> union(['norm_one', 'norm_inf']).

%%--------------------------------------------------------------------
%% Helper functions for running tests
%%--------------------------------------------------------------------

%% @doc
%% Runs all tests for the 50 evaluator implementations.
-spec run_tests() -> ok.
run_tests() -> run_tests(1).

%% @doc
%% Runs all tests for evaluator implementation number N and up to 50.
- spec run_tests(pos_integer()) -> ok.
run_tests(N) when N > 50 -> ok;
run_tests(N) ->
    run_test(N),
    run_tests(N + 1).

%% @doc
%% Runs all tests for evaluator implementation number N.
-spec run_test(pos_integer()) -> ok.
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
-spec run_specific_tests([pos_integer()]) -> ok.
run_specific_tests([]) -> ok;
run_specific_tests([N|Ns]) ->
    run_test(N),
    run_specific_tests(Ns).
