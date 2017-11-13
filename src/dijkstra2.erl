%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 23:25
%%% Optimization: Graph.delete
%%%-------------------------------------------------------------------
-module(dijkstra2).
-author("Steven").

-include("definitions.hrl").

%% API
-compile(export_all).

dijkstra(Graph, StartVertex) ->
  Q = pre(Graph, StartVertex),
  iteration(Graph, Q).

dijkstra(FileName, StartVertex, d) -> dijkstra_(FileName, StartVertex, d);
dijkstra(FileName, StartVertex, ud) -> dijkstra_(FileName, StartVertex, ud);
dijkstra(FileName, StartVertex, _) -> dijkstra_(FileName, StartVertex, d). % Anything else -> directed

dijkstra_(FileName, StartVertex, Variant) ->
  Graph = to_graph(FileName, Variant),
  Q = pre(Graph, StartVertex),
  iteration(Graph, Q).

to_graph(FileName, Variant) ->
  adtgraph:importG(FileName, Variant).

pre(?EMPTY_GRAPH, _) -> [];
pre(Graph, StartVertex) ->
  [Start | Rest] = startVertexAtFront(Graph, StartVertex),
  [{Start, 0, Start} | Rest].

startVertexAtFront(Graph, StartVertex) ->
  Vertices = adtgraph:getVertexes(Graph),
  HasVertex = lists:any(fun(Elem) -> Elem == StartVertex end, Vertices),
  if
    HasVertex -> [StartVertex | lists:delete(StartVertex, Vertices)];
    true -> Vertices
  end.

iteration(_, []) -> [];
iteration(Graph, Q) -> iteration_(Graph, Q, []).


iteration_(_, [], Result) -> Result;
iteration_(Graph, Q, Result) ->
  {SmallerQ, {VertH, EntfH, VorgH}} = pop_min(Q),

  NewQ = update_distance(Graph, adtgraph:getAdjacent(Graph, VertH), SmallerQ, VertH, EntfH, VorgH),
  NewGraph = adtgraph:deleteVertex(Graph, VertH),
  iteration_(NewGraph, NewQ, [{VertH, EntfH, VorgH} | Result]).

% An element is needed to be taken as the first min element, because otherwise it would be empty and the algorithm would
% put an empty value into the list when it finds a new min value (and it wouldn't compare them)
pop_min([{F1, F2, F3} | Rest]) -> pop_min_(Rest, [], {F1, F2, F3});
pop_min([F | Rest]) -> pop_min_(Rest, [], {infinite, F, undef}).

pop_min_([], NewQ, Min) -> {NewQ, Min};
pop_min_([{Vertex, Entf, Vorg} | Rest], NewQ, {OldV, OldEntf, OldVorg})
  when OldEntf > Entf ->
  pop_min_(Rest, [{OldV, OldEntf, OldVorg} | NewQ], {Vertex, Entf, Vorg});
pop_min_([Elem | Rest], NewQ, Min) ->
  pop_min_(Rest, [Elem | NewQ], Min).

update_distance(_, [], Q, _, _, _) -> Q;
update_distance(Graph, [AdjacentJ | Adjacent], Q, VertH, EntfH, VorgH) ->
  {NewQ, {_, EntfJ, VorgJ}} = pop(Q, AdjacentJ),

  Lhj = adtgraph:getValE(Graph, {VertH, AdjacentJ}, weight),
  if
    is_atom(Lhj) ->
      update_distance(Graph, Adjacent, [{AdjacentJ, EntfJ, VorgJ} | NewQ], VertH, EntfH, VorgH);
    EntfJ > EntfH + Lhj ->
      update_distance(Graph, Adjacent, [{AdjacentJ, EntfH + Lhj, VertH} | NewQ], VertH, EntfH, VorgH);
    true ->
      update_distance(Graph, Adjacent, [{AdjacentJ, EntfJ, VorgJ} | NewQ], VertH, EntfH, VorgH)
  end.


pop(List, Elem) -> pop(List, [], Elem).

pop([{Elem, Entf, Vorg} | Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, Entf, Vorg}};
pop([Elem | Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, infinite, undef}};
pop([F | Rest], Popped, Elem) -> pop(Rest, [F | Popped], Elem).
