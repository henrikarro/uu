-module(dfs).

-export([complete_binary_tree/1, search/2]).

complete_binary_tree(Depth) -> complete_binary_tree(Depth, 1, 1).

complete_binary_tree(MaxDepth, MaxDepth, Label) -> {Label, {}, {}};
complete_binary_tree(MaxDepth, CurrentDepth, Label) ->
    {Label, complete_binary_tree(MaxDepth, CurrentDepth + 1, Label * 2),
     complete_binary_tree(MaxDepth, CurrentDepth + 1, Label * 2 + 1)}.

search({}, _N) -> throw(not_found);
search({Label, _Left, _Right}, N) when Label =:= N -> [Label];
search({Label, Left, Right}, N) ->
    try
        [Label|search(Left, N)]
    catch
        not_found ->
            [Label|search(Right, N)]
    end.
