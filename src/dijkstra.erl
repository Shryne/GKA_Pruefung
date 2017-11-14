-module(dijkstra).
-author("Steven").

-include("definitions.hrl").

-export([dijkstra/2, dijkstra/3]).

dijkstra(Graph, StartVertex) ->
  Vertices = adtgraph:getVertexes(Graph),
  HasVertex = lists:any(fun(Elem) -> Elem == StartVertex end, Vertices),
  if
    not HasVertex -> [];
    true ->
      Q = pre(Vertices, StartVertex),
      iteration(Graph, Q)
  end.

dijkstra(FileName, StartVertex, d) -> dijkstra_(FileName, StartVertex, d);
dijkstra(FileName, StartVertex, ud) -> dijkstra_(FileName, StartVertex, ud);
dijkstra(FileName, StartVertex, _) -> dijkstra_(FileName, StartVertex, d). % Anything else -> directed

dijkstra_(FileName, StartVertex, Variant) ->
  Graph = to_graph(FileName, Variant),
  Vertices = adtgraph:getVertexes(Graph),
  HasVertex = lists:any(fun(Elem) -> Elem == StartVertex end, Vertices),
  if
    not HasVertex -> [];
    true ->
      Q = pre(Vertices, StartVertex),
      iteration(Graph, Q)
  end.

to_graph(FileName, Variant) ->
  adtgraph:importG(FileName, Variant).

pre(?EMPTY_GRAPH, _) -> [];
pre(Vertices, StartVertex) ->
  [Start|Rest] = startVertexAtFront(Vertices, StartVertex),
  [{Start, 0, Start}|Rest].

startVertexAtFront(Vertices, StartVertex) -> [StartVertex|lists:delete(StartVertex, Vertices)].

iteration(_, []) -> [];
iteration(Graph, Q) -> iteration_(Graph, Q, []).


iteration_(_, [], Result) -> Result;
iteration_(Graph, Q, Result) ->
  %io:fwrite(lists:append(["Q: ", util:to_String(Q), "\n"])),
  {SmallerQ, {VertH, EntfH, VorgH}} = pop_min(Q),

  NewQ = update_distance(Graph, adtgraph:getAdjacent(Graph, VertH), SmallerQ, VertH, EntfH, VorgH),
  iteration_(Graph, NewQ, [{VertH, EntfH, VorgH}|Result]).

% An element is needed to be taken as the first min element, because otherwise it would be empty and the algorithm would
% put an empty value into the list when it finds a new min value (and it wouldn't compare them)
pop_min([{F1, F2, F3}|Rest]) -> pop_min_(Rest, [], {F1, F2, F3});
pop_min([F|Rest]) -> pop_min_(Rest, [], {F, infinite, undef}).

pop_min_([], NewQ, Min) -> {NewQ, Min};
pop_min_([{Vertex, Entf, Vorg}|Rest], NewQ, {OldV, OldEntf, OldVorg})
  when OldEntf > Entf ->
  pop_min_(Rest, [{OldV, OldEntf, OldVorg}|NewQ], {Vertex, Entf, Vorg});
pop_min_([Elem|Rest], NewQ, Min) ->
  pop_min_(Rest, [Elem|NewQ], Min).

update_distance(_, [], Q, _, _, _) -> Q;
update_distance(Graph, [AdjacentJ|Adjacent], Q, VertH, EntfH, VorgH) ->
  {NewQ, QJ} = pop(Q, AdjacentJ),

  if
    QJ =/= nil ->
      {_, EntfJ, VorgJ} = QJ,
      Lhj = adtgraph:getValE(Graph, {VertH, AdjacentJ}, weight),
      if
        is_atom(Lhj) ->
          update_distance(Graph, Adjacent, [{AdjacentJ, EntfJ, VorgJ}|NewQ], VertH, EntfH, VorgH);
        EntfJ > EntfH + Lhj ->
          update_distance(Graph, Adjacent, [{AdjacentJ, EntfH + Lhj, VertH}|NewQ], VertH, EntfH, VorgH);
        true ->
          update_distance(Graph, Adjacent, [{AdjacentJ, EntfJ, VorgJ}|NewQ], VertH, EntfH, VorgH)
      end;
    true -> update_distance(Graph, Adjacent, NewQ, VertH, EntfH, VorgH)
  end.


pop(List, Elem) -> pop(List, [], Elem).

pop([], Q, _) -> {Q, nil}; % Vertex is true based on OK
pop([{Elem, Entf, Vorg}|Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, Entf, Vorg}};
pop([Elem|Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, infinite, undef}};
pop([F|Rest], Popped, Elem) -> pop(Rest, [F|Popped], Elem).
