%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2017 20:39
%%%-------------------------------------------------------------------
-module(bellmannford).
-author("Steven").

-include("definitions.hrl").

%% API
-export([bellmannford/3]).


bellmannford(FileName, StartVertex, d) -> bellmannford_(FileName, StartVertex, d);
bellmannford(FileName, StartVertex, ud) -> bellmannford_(FileName, StartVertex, ud);
bellmannford(FileName, StartVertex, _) -> bellmannford_(FileName, StartVertex, d). % Anything else -> directed

bellmannford_(FileName, StartVertex, Variant) ->
  Graph = adtgraph:importG(FileName, Variant),
  Vertices = startVertexAtFront(Graph, StartVertex),
  Q = pre(Vertices),
  NewQ = iteration(Graph, Q),
  check(FileName, Graph, NewQ),
  NewQ.

pre([]) -> [];
pre([Start|Rest]) -> [{Start, 0, Start}|Rest].

startVertexAtFront(Graph, StartVertex) ->
  Vertices = adtgraph:getVertexes(Graph),
  HasVertex = lists:any(fun(Elem) -> Elem == StartVertex end, Vertices),
  if
    HasVertex -> [StartVertex|lists:delete(StartVertex, Vertices)];
    true -> Vertices
  end.

iteration(?EMPTY_GRAPH, _) -> [];
iteration(Graph, Q) -> iteration_(Graph, Q, length(adtgraph:getVertexes(Graph)) - 1).

iteration_(__, Q, 0) -> Q;
iteration_(Graph, Q, N) ->
  NewQ = vertices_iteration(Graph, Q),
  iteration_(Graph, NewQ, N - 1).


check(_, ?EMPTY_GRAPH, _) -> [];
check(GraphPath, Graph, Q) -> check_(GraphPath, Graph, Q, Q).

check_(_, _, Q, []) -> Q;
check_(GraphPath, Graph, Q, [{Vi, Entfi, _}|Rest]) ->
  %io:fwrite(lists:append("Q: ", util:to_String(Q), "\n")),
  HasNegativeCircle = check_negative_circle(Graph, Vi, Entfi, Q, adtgraph:getAdjacent(Graph, Vi)),
  if
    HasNegativeCircle ->
      util:logging(lists:append([?LOGGING_FOLDER, "bellmannford_negative_circle.log"]),
        lists:append(["Negative circle found on ", GraphPath])),
      Q;
    true -> check_(GraphPath, Graph, Q, Rest)
  end.

vertices_iteration(Graph, Q) -> vertices_iteration_(Graph, Q, Q).

vertices_iteration_(_, [], Q) -> Q;
vertices_iteration_(Graph, [{Vi, Entfi, _}|Rest], Q) ->
  NewQ = distance_update(Graph, Vi, Entfi, Q, adtgraph:getAdjacent(Graph, Vi)),
  vertices_iteration_(Graph, Rest, NewQ);
vertices_iteration_(Graph, [_|Rest], Q) ->
  vertices_iteration_(Graph, Rest, Q).


distance_update(_, _, _, Q, []) -> Q;
distance_update(Graph, Vi, Entfi, Q, [Vj|Rest]) ->
  {NewQ, {_, Entfj, Vorgj}} = pop(Q, Vj),

  Lij = adtgraph:getValE(Graph, {Vi, Vj}, weight),

  if
    Entfj > Entfi + Lij ->
      distance_update(Graph, Vi, Entfi, [{Vj, Entfi + Lij, Vi}|NewQ], Rest);
    true ->
      distance_update(Graph, Vi, Entfi, [{Vj, Entfj, Vorgj}|NewQ], Rest)
  end.


check_negative_circle(_, _, _, _, []) -> false;
check_negative_circle(Graph, Vi, Entfi, Q, [Vj|Rest]) ->
  Entfj = getEntf(Q, Vj),
  Lij = adtgraph:getValE(Graph, {Vi, Vj}, weight),

  if
    Entfj > Entfi + Lij -> true;
    true ->
      distance_update(Graph, Vi, Entfi, Q, Rest)
  end.


getEntf([{Elem, Entf, _}|_], Elem) -> Entf;
getEntf([_|Rest], Elem) -> getEntf(Rest, Elem).


pop(List, Elem) -> pop(List, [], Elem).

pop([{Elem, Entf, Vorg}|Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, Entf, Vorg}};
pop([Elem|Rest], Popped, Elem) -> {lists:append(Popped, Rest), {Elem, infinite, undef}};
pop([F|Rest], Popped, Elem) -> pop(Rest, [F|Popped], Elem).
