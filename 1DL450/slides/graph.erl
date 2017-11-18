-module(graph).
-export([prop_graph/1, prop_graph_sa/1]).

-include_lib("proper/include/proper.hrl").

-type vertex():: pos_integer().
-type edge()  :: {vertex(), vertex()}.
-type graph() :: {[vertex()], [edge()]}.

edge(V) ->
  ?SUCHTHAT({V1,V2}, {oneof(V),oneof(V)}, V1 =< V2).

graph(N) ->
  V = lists:seq(1, N),
  ?LET(E, list(edge(V)),
       {V, lists:usort(E)}).

prop_graph(N) ->
  ?FORALL(G, graph(N), graph_length_from_sinc(G) < (N div 2)).

prop_graph_sa(N) ->
  ?FORALL_SA(G, ?TARGET(graph_sa(N)),
	     begin
		UV = graph_length_from_sinc(G),
		?MAXIMIZE(UV),
		UV < (N div 2)
	     end).


graph_sa(N) ->
  #{first => graph(N),
    next => fun (Base, _T) -> graph_next(Base) end}.

graph_next(G) ->
  Size = graph_size(G),
  ?LET(NewSize, neighbor_integer(Size),
       ?LET(Additional, neighbor_integer(Size div 10),
            begin
              {Removals, Additions} = case NewSize < Size of
                                        true ->
                                          N_Del = Additional + (Size - NewSize),
                                          N_Add = Additional,
                                          {N_Del, N_Add};
                                        false ->
                                          N_Del = Additional,
                                          N_Add = Additional  + (NewSize - Size),
                                          {N_Del, N_Add}
                                      end,
              ?LET(G_Del, remove_n_edges(G, Removals),
                   add_n_edges(G_Del, Additions))
            end)).

graph_size({_, E}) ->
  length(E).

%% generator for neighboring integer
neighbor_integer(Base) ->
  OffsetRange = trunc(0.05 * Base) +1,
  ?LET(X, integer(Base - OffsetRange, Base + OffsetRange), max(0, X)).

add_n_edges({V, E}, N) ->
  ?LET(NewEdges, vector(N, edge(V)),
       {V, lists:usort(E ++ NewEdges)}).

remove_n_edges({V, E}, 0) -> {V, E};
remove_n_edges({V, []}, _) -> {V, []};
remove_n_edges({V, E}, N) ->
  ?LET(Edge, oneof(E),
       ?LAZY(remove_n_edges({V, lists:delete(Edge, E)}, N - 1))).



%%---------------------------------------------------------------------
%%  Auxiliary functions below
%%---------------------------------------------------------------------

-spec graph_length_from_sinc(graph()) -> non_neg_integer().
graph_length_from_sinc({[], _}) -> 0;
graph_length_from_sinc({[R|_], E}) ->
  EdgeDict = edges_to_dict(E),
  GreyD = [{R, 0}],
  Grey = sets:from_list([R]),
  Black = sets:new(),
  length_from_sinc(GreyD, Grey, Black, EdgeDict, []).

length_from_sinc([], _, _, _, Acc) ->
  lists:foldr(fun ({_, D}, Max) ->
                  case D>Max of
                    true -> D;
                    _ -> Max
                  end
              end, 0, Acc);
length_from_sinc([{Curr, CurrD}|Rem], Grey, Black, Edges, Acc) ->
  case dict:is_key(Curr, Edges) of
    true ->
      Neighbors = dict:fetch(Curr, Edges),
      NewNeighbors = lists:filter(fun (V) -> not sets:is_element(V, Black) andalso not sets:is_element(V, Grey) end, Neighbors),
      NeighborsWithD = [{N, CurrD+1} || N <- NewNeighbors],
      length_from_sinc(Rem ++ NeighborsWithD,
                       sets:union(Grey, sets:from_list(NewNeighbors)),
                       sets:add_element(Curr, Black),
                       Edges,
                       Acc ++ NeighborsWithD);
    false ->
      length_from_sinc(Rem, Grey, sets:add_element(Curr, Black), Edges, Acc)
  end.

edges_to_dict(Edges) ->
  lists:foldr(fun ({L, R}, AccIn) ->
                  D = dict:append(L, R, AccIn),
                  dict:append(R, L, D)
              end,
              dict:new(), Edges).

