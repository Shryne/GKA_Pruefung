%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Okt 2017 21:39
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("Steven").
% Notiz: weight ist der name

%% API
-compile(export_all).

dijkstra(Filename, _StartVertex, d) ->
  Graph = to_graph(Filename, d),
  dijkstra_pre(Graph);

dijkstra(_Filename, _StartVertex, ud) ->
  nil.

to_graph(FileName, Variant) ->
  adtgraph:importG(FileName, Variant).

dijkstra_pre(Graph) ->
  Vertices = adtgraph:getVertexes(Graph),
  VertexAmount = length(Vertices),
  OK = lists:duplicate(VertexAmount, false),
  Entf = [0|lists:duplicate(VertexAmount - 1, infinite)],
  Vorg = [0|lists:duplicate(VertexAmount - 1, undef)],
  file:delete("Log"),
  dijkstra_iteration(Graph, Vertices, OK, Entf, Vorg).

dijkstra_iteration(Graph, Vertices, OK, Entf, Vorg) ->
  print(["--------------ITER--------------"]),
  print(["OK: ", util:list2string(OK)]),
  print(["Entf: ", util:list2string(Entf)]),
  print(["Vorg: ", util:list2string(Vorg)]),
  {H, Entfh} = dijkstra_min(OK, Entf, inf, 0, inf),
  NewOK = dijkstra_ok_true(OK, [], H, 0),

  {AdjacentFalse, EntfFalse, VorgFalse} = dijkstra_adjacent_false(Graph, Vertices, H, NewOK, Entf, Vorg),
  print(["AdjacentFalse: ", util:list2string(AdjacentFalse)]),
  print(["EntfFalse: ", util:list2string(EntfFalse)]),
  print(["VorgFalse: ", util:list2string(VorgFalse)]),
  {NewEntf, NewVorg} = dijkstra_distance(Graph, AdjacentFalse, EntfFalse, VorgFalse, H, Entfh, {[], []}),
  AllTrue = lists:all(fun(Elem) -> Elem == true end, NewOK),
  if
    AllTrue -> {NewEntf, NewVorg, NewOK};
    true -> dijkstra_iteration(Graph, Vertices, NewOK, NewEntf, NewVorg)
  end.


dijkstra_min([], [], _, _, Result) -> Result;
dijkstra_min([false|RestOK], [Entfi|RestEntf], Min, I, _) when Min > Entfi ->
  dijkstra_min(RestOK, RestEntf, Entfi, I + 1, {I, Entfi});
dijkstra_min([_|RestOK], [_|RestEntf], Min, I, Result) ->
  dijkstra_min(RestOK, RestEntf, Min, I + 1, Result).

dijkstra_ok_true([_|Rest], Result, I, I) -> lists:append(lists:reverse(Result), [true|Rest]);
dijkstra_ok_true([F|Rest], Result, H, I) -> dijkstra_ok_true(Rest, [F|Result], H, I + 1).

dijkstra_adjacent_false(Graph, Vertices, H, OK, Entf, Vorg) ->
  Adjacent = adtgraph:getAdjacent(Graph, H),
  dijkstra_adjacent_false_(Adjacent, Vertices, OK, Entf, Vorg, {[], [], []}).

dijkstra_adjacent_false_([], _, _, _, _, {ResultOK, ResultEntf, ResultVorg}) ->
  {lists:reverse(ResultOK), lists:reverse(ResultEntf), lists:reverse(ResultVorg)};
dijkstra_adjacent_false_([Adjacenti|Adjacent], Vertices, OK, Entf, Vorg, {ResultOK, ResultEntf, ResultVorg}) ->
  AdjacentIndex = index_of(Adjacenti, Vertices),
  OKElem = get(OK, AdjacentIndex),
  if
    OKElem == false ->
      EntfElem = get(Entf, AdjacentIndex),
      VorgElem = get(Vorg, AdjacentIndex),
      dijkstra_adjacent_false_(Adjacent, Vertices, OK, Entf, Vorg, {[OKElem|ResultOK], [EntfElem|ResultEntf], [VorgElem|ResultVorg]});
    true ->
      dijkstra_adjacent_false_(Adjacent, Vertices, OK, Entf, Vorg, {ResultOK, ResultEntf, ResultVorg})
  end.

dijkstra_distance(_, [], [], [], _, _, {EntfResult, VorgResult}) -> {lists:reverse(EntfResult), lists:reverse(VorgResult)};
dijkstra_distance(Graph, [VertexI|Adjacent], [Entfj|RestEntf], [Vorgj|RestVorg], VertexH, Entfh, {NewEntf, NewVorg}) ->
  Lhj = adtgraph:getValE(Graph, VertexH, VertexI, weight),
  print(["Lhj[", VertexH, VertexI, "]", Lhj]),
  if
    Entfj > Entfh + Lhj ->
      NewEntfj = Entfh + Lhj,
      NewVorgj = VertexH,
      dijkstra_distance(Graph, Adjacent, RestEntf, RestVorg, VertexH, Entfh, {[NewEntfj|NewEntf], [NewVorgj|NewVorg]});
    true ->
      dijkstra_distance(Graph, Adjacent, RestEntf, RestVorg, VertexH, Entfh, {[Entfj|NewEntf], [Vorgj|NewVorg]})
  end.


index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

get(List, Index) -> get(List, 0, Index).

get([Elem|_], Counter, Counter) -> Elem;
get([_|Rest], Counter, Index) -> get(Rest, Counter + 1, Index).


print(List) -> util:logging("Log", lists:concat([List, "\n"])).