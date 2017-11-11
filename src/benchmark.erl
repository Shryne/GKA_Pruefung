%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Nov 2017 17:42
%%%-------------------------------------------------------------------
-module(benchmark).
-author("Steven").

-define(BENCHMARK_FOLDER, "benchmark/").
-define(BENCHMARK_FILE_TYPE, ".csv").
-define(TIME_FORMAT, millisecond).
-define(MAX_TIME_PER_BENCHMARK, 10000).

%% API
-export([start/3]).

start(_, Modules, ArgumentList)
  when length(Modules) =/= length(ArgumentList) ->
  nil;
start(FileName, Modules, FolderList) ->
  filelib:ensure_dir(?BENCHMARK_FOLDER),
  file:delete(FileName),
  LogFileName = lists:append([?BENCHMARK_FOLDER, FileName, ?BENCHMARK_FILE_TYPE]),
  module_benchmarks(Modules, FolderList, []).

% [dijkstra, ...], [[{"graph...", 12, d}, ...], ...]
module_benchmarks(_, [], Result) -> Result;
module_benchmarks([], _, Result) -> Result;
module_benchmarks([TopModule|RestModules], Folder, Result) ->
  {ok, DataInFolder} = file:list_dir(Folder),
  Graphs = lists:filter(fun(A) -> lists:suffix(".graph", A) end, DataInFolder),
  module_benchmarks(RestModules, Graphs,
    lists:append(Result, [{TopModule, module_benchmark(TopModule, Graphs, Folder, [])}])
  ).


module_benchmark(_, [], _, Result) -> Result;
module_benchmark(Module, [GraphFile|Rest], Folder, Result) ->
  GraphPath = lists:append([Folder, GraphFile]),
  StartTime = erlang:system_time(?TIME_FORMAT),
  Module:dijkstra(GraphPath, 0, ud),
  TimeNeeded = erlang:system_time(?TIME_FORMAT) - StartTime,
  %if
    %TimeNeeded > ?MAX_TIME_PER_BENCHMARK -> lists:append([Result, [{GraphFile, TimeNeeded}]]);
    module_benchmark(Module, Rest, Folder, lists:append([Result, [{GraphFile, TimeNeeded}]])).
  %end.