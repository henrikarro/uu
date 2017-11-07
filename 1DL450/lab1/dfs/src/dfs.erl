-module(dfs).

-export([complete_binary_tree/1, search/2]).

-type tree(T) :: empty_tree(T) | non_empty_tree(T).
-type empty_tree(_T) :: {}.
-type non_empty_tree(T) :: {T, tree(T), tree(T)}.

-spec complete_binary_tree(pos_integer()) -> non_empty_tree(pos_integer()).
complete_binary_tree(Depth) -> complete_binary_tree(Depth, 1, 1).

%-spec complete_binary_tree(pos_integer(), pos_integer(), L) -> non_empty_tree(L) when L :: pos_integer().
complete_binary_tree(MaxDepth, MaxDepth, Label) -> {Label, {}, {}};
complete_binary_tree(MaxDepth, CurrentDepth, Label) ->
    {Label, complete_binary_tree(MaxDepth, CurrentDepth + 1, Label * 2),
     complete_binary_tree(MaxDepth, CurrentDepth + 1, Label * 2 + 1)}.

-spec search(non_empty_tree(L), any()) -> [L, ...].
search({}, _N) -> throw(not_found);
search({Label, _Left, _Right}, N) when Label =:= N -> [Label];
search({Label, Left, Right}, N) ->
    try
        [Label|search(Left, N)]
    catch
        not_found ->
            [Label|search(Right, N)]
    end.
