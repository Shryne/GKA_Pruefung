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

dijkstra(Filename, _StartVertex, ud) ->
  Graph = to_graph(Filename, ud),
  dijkstra_pre(Graph).

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

  %{AdjacentFalse, EntfFalse, VorgFalse} = dijkstra_adjacent_false(Graph, Vertices, H, NewOK, Entf, Vorg),
  %print(["AdjacentFalse: ", util:list2string(AdjacentFalse)]),
  %print(["EntfFalse: ", util:list2string(EntfFalse)]),
  %print(["VorgFalse: ", util:list2string(VorgFalse)]),
  print(["dijkstra_distance"]),
  {NewEntf, NewVorg} = dijkstra_distance(Graph, Vertices, adtgraph:getAdjacent(Graph, get(Vertices, H)), Entf, Vorg, OK, get(Vertices, H), Entfh),
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
  print(["H: ", util:to_String(get(Vertices, H))]),
  Adjacent = adtgraph:getAdjacent(Graph, get(Vertices, H)),
  print(["Adjacent to H: ", util:list2string(Adjacent)]),
  dijkstra_adjacent_false_(Adjacent, Vertices, OK, Entf, Vorg, {[], [], []}).

dijkstra_adjacent_false_([], _, _, _, _, {ResultOK, ResultEntf, ResultVorg}) ->
  {lists:reverse(ResultOK), lists:reverse(ResultEntf), lists:reverse(ResultVorg)};
dijkstra_adjacent_false_([Adjacenti|Adjacent], Vertices, OK, Entf, Vorg, {ResultOK, ResultEntf, ResultVorg}) ->
  AdjacentIndex = index_of(Adjacenti, Vertices),
  print(["AdjacentIndex: ", util:to_String(AdjacentIndex)]),
  print(["Vertices: ", util:list2string(Vertices)]),
  OKElem = get(OK, AdjacentIndex),
  print(["OKElem: ", OKElem]),
  if
    OKElem == false ->
      EntfElem = get(Entf, AdjacentIndex),
      VorgElem = get(Vorg, AdjacentIndex),
      dijkstra_adjacent_false_(Adjacent, Vertices, OK, Entf, Vorg, {[OKElem|ResultOK], [EntfElem|ResultEntf], [VorgElem|ResultVorg]});
    true ->
      dijkstra_adjacent_false_(Adjacent, Vertices, OK, Entf, Vorg, {ResultOK, ResultEntf, ResultVorg})
  end.

dijkstra_distance(_, _, [], Entf, Vorg, _, _, _) -> {Entf, Vorg};
dijkstra_distance(Graph, Vertices, [VertexI|Adjacent], Entf, Vorg, OK, VertexH, Entfh) ->
  AdjacentIndex = index_of(VertexI, Vertices),
  print(["AdjacentIndex: ", util:to_String(AdjacentIndex)]),
  print(["Vertices: ", util:list2string(Vertices)]),
  OKElem = get(OK, AdjacentIndex),

  if
    OKElem == false ->
      Lhj = adtgraph:getValE(Graph, {VertexH, VertexI}, weight),
      Entfj = get(Entf, AdjacentIndex),
      print(["Lhj[", util:to_String(VertexH), " -> ", util:to_String(VertexI), "]: ", util:to_String(Lhj)]),
      print(["Entfj: ", util:to_String(Entfj)]),
      print(["Entfh: ", util:to_String(Entfh)]),
      if
        is_atom(Lhj) ->
          dijkstra_distance(Graph, Vertices, Adjacent, Entf, Vorg, OK, VertexH, Entfh);
        Entfj > Entfh + Lhj ->
          NewEntf = set(Entf, AdjacentIndex, Entfh + Lhj),
          NewVorg = set(Vorg, AdjacentIndex, VertexH),
          print(["NewEntf: ", util:list2string(NewEntf)]),
          print(["NewVorg: ", util:list2string(NewVorg)]),
          dijkstra_distance(Graph, Vertices, Adjacent, NewEntf, NewVorg, OK, VertexH, Entfh);
        true ->
          dijkstra_distance(Graph, Vertices, Adjacent, Entf, Vorg, OK, VertexH, Entfh)
      end;
    true ->
      dijkstra_distance(Graph, Vertices, Adjacent, Entf, Vorg, OK, VertexH, Entfh)
  end.


index_of(Item, List) -> index_of(Item, List, 0).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

get(List, Index) -> get(List, 0, Index).

get([Elem|_], Counter, Counter) -> Elem;
get([_|Rest], Counter, Index) -> get(Rest, Counter + 1, Index).

set(List, Index, Elem) -> set(List, [], 0, Index, Elem).

set([_|Rest], Result, Counter, Counter, Elem) -> lists:append(lists:reverse([Elem|Result]), Rest);
set([E|Rest], Result, Counter, Index, Elem) -> set(Rest, [E|Result], Counter + 1, Index, Elem).

print(List) -> util:logging("Log", lists:concat([List, "\n"])).

measure(F) ->
  B = now(),
  V = F(),
  A = now(),
  {timer:now_diff(A,B), V}.