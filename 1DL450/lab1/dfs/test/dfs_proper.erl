-module(dfs_proper).

-include_lib("proper/include/proper.hrl").

%%%
%%% PropEr Properties
%%%

prop_depth_is_correct() ->
    ?FORALL(D, depth(), tree_depth(dfs:complete_binary_tree(D)) =:= D).

prop_size_is_correct() ->
    ?FORALL(D, depth(), tree_size(dfs:complete_binary_tree(D)) =:= round(math:pow(2, D)) - 1).

prop_sum_of_labels_is_correct() ->
    ?FORALL(D, depth(), sum_of_labels_is_correct(D, dfs:complete_binary_tree(D))).

sum_of_labels_is_correct(Depth, Tree) ->
    L = tree_to_inorder_list(Tree),
    SumOfLabels = lists:foldl(fun(X, Sum) -> X + Sum end, 0, L),
    MaxLabel = round(math:pow(2, Depth)) - 1,
    SumOfLabels =:= round((MaxLabel * (MaxLabel + 1)) / 2).

prop_search_is_correct() ->
    ?FORALL({D, N}, depth_and_label(),
            dfs:search(dfs:complete_binary_tree(D), N) =:= solution(N)).

solution(1) -> [1];
solution(N) when N > 1 -> solution(N div 2) ++ [N].

prop_search_result_is_within_range() ->
    ?FORALL({D, N}, depth_and_label(), search_result_is_within_range(D, N)).

search_result_is_within_range(Depth, N) ->
    Tree = dfs:complete_binary_tree(Depth),
    Result = dfs:search(Tree, N),
    Result =:= lists:filter(fun(X) -> X > 0 andalso X < math:pow(2, Depth) end, Result).

prop_search_result_is_sorted() ->
    ?FORALL({D, N}, depth_and_label(), is_sorted(dfs:search(dfs:complete_binary_tree(D), N))).

is_sorted(Xs) ->
    lists:sort(Xs) =:= Xs.

%%%
%%% PropEr Generators
%%%

depth() ->
    ?SUCHTHAT(D, pos_integer(), D < 20).

depth_and_label() ->
    ?SUCHTHAT({D, N}, {depth(), pos_integer()}, N < math:pow(2, D)).

%%%
%%% Tree Helper Functions
%%%

tree_size({}) -> 0;
tree_size({_, Left, Right}) -> 1 + tree_size(Left) + tree_size(Right).

tree_depth({}) -> 0;
tree_depth({_, Left, Right}) -> 1 + max(tree_depth(Left), tree_depth(Right)).

tree_to_inorder_list({}) -> [];
tree_to_inorder_list({Label, Left, Right}) ->
    tree_to_inorder_list(Left) ++ [Label] ++ tree_to_inorder_list(Right).
