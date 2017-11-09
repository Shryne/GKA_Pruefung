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

-include("Definitions.hrl").

-define(MEASUREMENT_FOLDERS, lists:append(?MEASUREMENT_FOLDER, "/dijkstra/")).
-define(LOG_FOLDERS, lists:append(?LOGGING_FOLDER, "/dijkstra/")).
-define(MEASUREMENT_FILE_TYPE, ".csv").
-define(LOG_FILE_TYPE, ".log").

-export([dijkstra/3]).

dijkstra(FileName, StartVertex, d) -> dijkstra_(FileName, StartVertex, d);
dijkstra(FileName, StartVertex, ud) -> dijkstra_(FileName, StartVertex, ud);
dijkstra(FileName, StartVertex, _) -> dijkstra_(FileName, StartVertex, nil).

dijkstra_(FileName, StartVertex, Variant) ->
  HasFolder = lists:any(fun(Elem) -> Elem == "/" end, FileName),
  GraphName =
    if
    HasFolder -> % Filters the folders from the filename
      lists:dropwhile(fun(_) ->
        lists:any(fun(Elem) -> Elem == "/", FileName end, FileName) end,
        FileName
      );
    true -> FileName
    end,

  MeasurementPath = lists:append([?MEASUREMENT_FOLDERS, GraphName, ?MEASUREMENT_FILE_TYPE]),
  LogPath = lists:append([?LOG_FOLDERS, GraphName, ?LOG_FILE_TYPE]),
  filelib:ensure_dir(?MEASUREMENT_FOLDERS),
  filelib:ensure_dir(?LOG_FOLDERS),
  file:delete(LogPath),
  file:delete(MeasurementPath),

  u:log(LogPath, [
    "Dijkstra start mit FileName: ", util:to_String(FileName),
    " StartVertex: ", util:to_String(StartVertex),
    " Variant: ", util:to_String(Variant)
  ]
  ),
  Graph = adtgraph:importG(FileName, Variant),

  u:log(LogPath, ["Folgenden Graph geladen:\n", util:list2string(adtgraph:getVertexes(Graph)), "\n"]),
  measured(LogPath, MeasurementPath, StartVertex, Graph).

% Case: Graph couldn't be loaded. This would result in empty OK, ... lists and there wouldn't be any calculation to be
% done.
measured(_, _, _, {{}, [], []}) -> {[], [], []};
measured(LogPath, MeasurementPath, StartVertex, Graph) ->
  u:log(LogPath, ["Korrekten Graph erhalten"]),
  u:measure(MeasurementPath,
    fun() ->
      u:log(LogPath, ["Starte Messung"]),
      % I swap the StartVertex from pos x to the front, because that way it's much easier to create the
      % OK... lists with the special initialization for the StartVertex. If the StartVertex isn't inside
      % the graph, the first Vertex of the Graph is assumed to be the StartVertex
      Vertices = startVertexAtFront(Graph, StartVertex),
      {OK, Entf, Vorg} = preparation(LogPath, Vertices),
      iteration(LogPath, Graph, Vertices, OK, Entf, Vorg)
    end,
    1
  ).

startVertexAtFront(Graph, StartVertex) ->
  Vertices = adtgraph:getVertexes(Graph),
  HasVertex = lists:any(fun(Elem) -> Elem == StartVertex end, Vertices),
  if
    HasVertex -> lists:append([StartVertex], lists:delete(StartVertex, Vertices));
    true -> Vertices
  end.

% The StartVertex must be the head of the given list, because the other lists depend on it and they put the
% initialization for it in the head, too.
% === Vorbereitung
preparation(LogPath, Vertices) ->
  u:log(LogPath, ["--------------PRE--------------"]),
  VertexAmount = length(Vertices),

  % === Entfi gibt die bisher festgestellte kürzeste Entfernung von v1 nach vi an. Der Startwert ist 0 für i = 1 und inf
  % sonst.
  Entf = [0|lists:duplicate(VertexAmount - 1, infinite)],
  % === Vorgi gibt den Vorgänger von vi auf dem bisher kürzesten Weg von v1 nach vi an. Der Startwert ist v1 für i = 1
  % und undefiniert sonst.
  Vorg = [0|lists:duplicate(VertexAmount - 1, undef)],
  % === OKi gibt an, ob die kürzeste Entfernung von v1 nach vi schon bekannt ist. Der Startwert für alle Werte von i ist
  % false.
  OK = lists:duplicate(VertexAmount, false),

  u:log(LogPath, ["Graph enthält ", util:to_String(VertexAmount), " Anzahl von Vertices"]),
  {OK, Entf, Vorg}.

iteration(LogPath, Graph, Vertices, OK, Entf, Vorg) ->
  u:log(LogPath, ["--------------ITER--------------"]),
  u:log(LogPath, ["OK: ", u:toString(OK)]),
  u:log(LogPath, ["Entf: ", u:toString(Entf)]),
  u:log(LogPath, ["Vorg: ", u:toString(Vorg)]),

  {H, Entfh} = ok_min(OK, Entf, inf, 0, inf),
  u:log(LogPath, ["H ist: ", util:to_String(H), " mit Entf ", util:to_String(Entf)]),

  u:log(LogPath, ["Setze OK für neues H auf true"]),
  NewOK = ok_true(OK, [], H, 0),
  u:log(LogPath, ["Neues OK: ", u:toString(NewOK)]),

  u:log(LogPath, ["dijkstra_distance"]),
  {NewEntf, NewVorg} = distance(LogPath, Graph, Vertices,
    adtgraph:getAdjacent(Graph, get(Vertices, H)), Entf, Vorg, OK, get(Vertices, H), Entfh
  ),
  AllTrue = lists:all(fun(Elem) -> Elem == true end, NewOK),
  if
    AllTrue -> {NewEntf, NewVorg, NewOK};
    true -> iteration(LogPath, Graph, Vertices, NewOK, NewEntf, NewVorg)
  end.

% === Suche unter den Ecken vi mit OKi = false eine Ecke vh mit dem kleinsten Wert von Entfi
% Searches the seconds list for the smallest element that is false based on the first list and returns its index and
% value.
ok_min([], [], _, _, Result) -> Result;
ok_min([false|RestOK], [Entfi|RestEntf], Min, I, _) when Min > Entfi ->
  ok_min(RestOK, RestEntf, Entfi, I + 1, {I, Entfi});
ok_min([_|RestOK], [_|RestEntf], Min, I, Result) ->
  ok_min(RestOK, RestEntf, Min, I + 1, Result).

% === Setze OKh := true
ok_true([_|Rest], Result, I, I) -> lists:append(lists:reverse(Result), [true|Rest]);
ok_true([F|Rest], Result, H, I) -> ok_true(Rest, [F|Result], H, I + 1).

% === Für alle Ecken vj mit OKj = false, für die die Kante vhvj existiert:
distance(_, _, _, [], Entf, Vorg, _, _, _) -> {Entf, Vorg};
distance(LogPath, Graph, Vertices, [VertexI|Adjacent], Entf, Vorg, OK, VertexH, Entfh) ->
  AdjacentIndex = index_of(VertexI, Vertices),
  u:log(LogPath, ["AdjacentIndex: ", util:to_String(AdjacentIndex)]),
  u:log(LogPath, ["Vertices: ", u:toString(Vertices)]),
  OKElem = get(OK, AdjacentIndex),

  if
    OKElem == false ->
      Lhj = adtgraph:getValE(Graph, {VertexH, VertexI}, weight),
      Entfj = get(Entf, AdjacentIndex),
      u:log(LogPath, ["Lhj[", util:to_String(VertexH), " -> ", util:to_String(VertexI), "]: ", util:to_String(Lhj)]),
      u:log(LogPath, ["Entfj: ", util:to_String(Entfj)]),
      u:log(LogPath, ["Entfh: ", util:to_String(Entfh)]),
      if
        is_atom(Lhj) ->
          distance(LogPath, Graph, Vertices, Adjacent, Entf, Vorg, OK, VertexH, Entfh);
        % === Falls gilt Entfj > Entfh + lhj dann
        Entfj > Entfh + Lhj ->
          % === setze Entfj := Entfh + lhj
          NewEntf = set(Entf, AdjacentIndex, Entfh + Lhj),
          % === Setze Vorgj := h
          NewVorg = set(Vorg, AdjacentIndex, VertexH),
          u:log(LogPath, ["NewEntf: ", u:toString(NewEntf)]),
          u:log(LogPath, ["NewVorg: ", u:toString(NewVorg)]),
          distance(LogPath, Graph, Vertices, Adjacent, NewEntf, NewVorg, OK, VertexH, Entfh);
        true ->
          distance(LogPath, Graph, Vertices, Adjacent, Entf, Vorg, OK, VertexH, Entfh)
      end;
    true ->
      distance(LogPath, Graph, Vertices, Adjacent, Entf, Vorg, OK, VertexH, Entfh)
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