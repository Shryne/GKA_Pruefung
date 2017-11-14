-module(benchmark).
-author("Steven").

-define(BENCHMARK_FOLDER, "benchmark/").
-define(BENCHMARK_FILE_TYPE, ".csv").
-define(TIME_FORMAT, millisecond).
-define(MAX_TIME_PER_BENCHMARK, 10000).
-define(GRAPH_FILE_TYPE, ".graph").
-define(OUTPUT_DELIMITER, ";").

-define(MODULES, ["dijkstra", "bellmannford"]).
-define(FUNCTIONS, [
  fun(Graph, StartVertex) -> dijkstra3:dijkstra(Graph, StartVertex) end,
  fun(Graph, StartVertex) -> bellmannford:bellmannford(Graph, StartVertex) end
]).

%% API
%-export([start/3]).
-compile(export_all).

% Run: benchmark:start("benchmark", [dijkstra, dijkstra1, dijkstra2, dijkstra3, dijkstra3], "eigene_graphen/").
start(FileName, Folder, Variant) ->
  filelib:ensure_dir(?BENCHMARK_FOLDER),
  BenchmarkFile = lists:append([?BENCHMARK_FOLDER, FileName, ?BENCHMARK_FILE_TYPE]),
  file:delete(BenchmarkFile),
  u:log(BenchmarkFile, ["Benchmark\n"]),

  Graphs = importGraphsSorted(Folder),
  logHeader(BenchmarkFile, ?MODULES),
  module_benchmarks(BenchmarkFile, ?FUNCTIONS, Graphs, Folder, Variant).

% Returns all Graphs from the given folder in a sorted list. The sorting is based on a simple comparison of the file
% names and because of this it's important to use numbers with the same amount of digits. Otherwise comparing graph_3
% with graph_200 for example would be erroneous.
% Import format: graph_XXXX.
importGraphsSorted(Folder) ->
  {ok, DataInFolder} = file:list_dir(Folder),
  lists:filter(
    fun(A) -> lists:suffix(?GRAPH_FILE_TYPE, A) end,
    lists:sort(fun(A, B) -> A < B end, DataInFolder)
  ).

% Prints the header into the file (the top line of the table). The GraphNames should be sorted.
% Format: Modul; graph_1; graph_2; graph_3; ...\n
logHeader(_, []) -> nil;
logHeader(BenchmarkFile, [Module|Rest]) ->
  u:log(BenchmarkFile, ["Module", ?OUTPUT_DELIMITER, Module, logHeader_(BenchmarkFile, Rest, [])]).

logHeader_(_, [], Result) -> Result;
logHeader_(BenchmarkFile, [GraphName|Rest], Result) ->
  logHeader_(BenchmarkFile, Rest, lists:append([Result, ?OUTPUT_DELIMITER, GraphName])).

module_benchmarks(_, _, [], _, _) -> benchmark_done;
module_benchmarks(BenchmarkFile, Modules, [GraphFile|RestGraphs], Folder, Variant) ->
  GraphPath = lists:append([Folder, GraphFile]),
  Graph = adtgraph:importG(GraphPath, Variant),
  {Result, ModulesDone} = module_benchmark(Modules, [], Graph, Folder, []),
  u:log(BenchmarkFile, lists:append([util:to_String(GraphFile), Result])),
  module_benchmarks(BenchmarkFile, ModulesDone, RestGraphs, Folder, Variant).

module_benchmark([], ModulesDone, _, _, Result) -> {Result, lists:reverse(ModulesDone)};
module_benchmark([Module|RestModules], ModulesDone, Graph, Folder, Result) ->
  StartTime = erlang:system_time(?TIME_FORMAT),
  Module(Graph, 1),
  TimeNeeded = erlang:system_time(?TIME_FORMAT) - StartTime,
  if
    TimeNeeded > ?MAX_TIME_PER_BENCHMARK ->
      module_benchmark(RestModules, ModulesDone, Graph, Folder, lists:append([Result, ";", util:to_String(TimeNeeded)]));
    true -> module_benchmark(RestModules, [Module|ModulesDone], Graph, Folder, lists:append([Result, ";", util:to_String(TimeNeeded)]))
  end.