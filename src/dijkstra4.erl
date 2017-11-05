%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 23:25
%%% Optimization: MinTree
%%%-------------------------------------------------------------------
-module(dijkstra4).
-author("Steven").

%% API
-compile(export_all).

convert(Filename) ->
  Graph = to_graph(Filename, d),
  to_set(adtgraph:getVertexes(Graph), {0, nil}),
  adtgraph:getVertexes(Graph).

to_set([], Set) -> Set;
to_set([F|R], Set) -> to_set(R, gb_sets:insert(F, Set)).


dijkstra(Filename, _StartVertex, d) ->
  Graph = to_graph(Filename, d),
  dijkstra_pre(Graph);

dijkstra(Filename, _StartVertex, ud) ->
  Graph = to_graph(Filename, ud),
  dijkstra_pre(Graph).

to_graph(FileName, Variant) ->
  adtgraph:importG(FileName, Variant).

dijkstra_pre(Graph) ->
  [Start|Rest] = adtgraph:getVertexes(Graph),
  Q = [{Start, 0, start}|Rest],
  file:delete("Log"),
  dijkstra_iteration(Graph, Q, []).

dijkstra_iteration(_, [], Result) -> Result;
dijkstra_iteration(Graph, Q, Result) ->
  u:print(["-----------------Iteration----------------------\n"]),
  u:print(["Q: ", util:to_String(Q)]),
  {SmallerQ, {VertH, EntfH, VorgH}} = pop_min(Q),
  u:print(["VertH: ", util:to_String(VertH)]),
  u:print(["EntfH: ", util:to_String(EntfH)]),
  u:print(["VorgH: ", util:to_String(VorgH)]),

  u:print(["SmallerQ: ", util:to_String(SmallerQ)]),
  NewQ = update_distance(Graph, adtgraph:getAdjacent(Graph, VertH), SmallerQ, VertH, EntfH, VorgH),
  NewGraph = adtgraph:deleteVertex(Graph, VertH),
  u:print(["NewQ: ", util:to_String(NewQ)]),
  dijkstra_iteration(NewGraph, NewQ, [{VertH, EntfH, VorgH}|Result]).

% An element is needed to be taken as the first min element, because otherwise it would be empty and the algorithm would
% put an empty value into the list when it finds a new min value (and it wouldn't compare them)
pop_min([{F1, F2, F3}|Rest]) -> pop_min_(Rest, [], {F1, F2, F3});
pop_min([F|Rest]) -> pop_min_(Rest, [], {infinite, F, undef}).

pop_min_([], NewQ, Min) -> {NewQ, Min};
pop_min_([{Vertex, Entf, Vorg}|Rest], NewQ, {OldV, OldEntf, OldVorg})
  when OldEntf > Entf ->
  pop_min_(Rest, [{OldV, OldEntf, OldVorg}|NewQ], {Vertex, Entf, Vorg});
pop_min_([Elem|Rest], NewQ, Min) ->
  pop_min_(Rest, [Elem|NewQ], Min).

update_distance(_, [], Q, _, _, _) -> Q;
update_distance(Graph, [AdjacentJ|Adjacent], Q, VertH, EntfH, VorgH) ->
  {TempQ, {_, EntfJ, VorgJ}} = pop(Q, AdjacentJ),
  Lhj = adtgraph:getValE(Graph, {VertH, AdjacentJ}, weight),
  if
    is_atom(Lhj) ->
      update_distance(Graph, Adjacent, [{AdjacentJ, EntfJ, VorgJ}|TempQ], VertH, EntfH, VorgH);
    EntfJ > EntfH + Lhj ->
      update_distance(Graph, Adjacent, [{AdjacentJ, EntfH + Lhj, VertH}|TempQ], VertH, EntfH, VorgH);
    true ->
      update_distance(Graph, Adjacent, [{AdjacentJ, EntfJ, VorgJ}|TempQ], VertH, EntfH, VorgH)
  end.

pop(List, Elem) -> pop(List, [], Elem).

pop([{Elem, Entf, Vorg}|Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, Entf, Vorg}};
pop([Elem|Rest], Popped, Elem) -> {lists:append(Popped, Rest), as_tuple(Elem)};
pop([F|Rest], Popped, Elem) -> pop(Rest, [F|Popped], Elem).

as_tuple({Vert, Entf, Vorg}) -> {Vert, Entf, Vorg};
as_tuple(Vert) -> {Vert, infinite, undef}.