%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 23:48
%%%-------------------------------------------------------------------
-module(u).
-author("Steven").

-define(MEASURE_FILE_NAME, "measurement.log").

%% API
-compile(export_all).

print(Name, List) -> util:logging(Name, lists:concat([List, "\n"])).


measure(Function, Amount) ->
  {Best, Worst, All} = measure_(Function, 0, inf, 0, Amount),
  print(?MEASURE_FILE_NAME,
    ["Measurements done. Avg: ", All / Amount, ", Best: ", Best, ", Worst: ", Worst]
  ).

measure_(_, Result, 0) -> Result.
measure_(Function, Result, Best, Worst, Amount) ->
  print(?MEASURE_FILE_NAME,
    ["Measurement number ", util:to_String(Amount), " starting..."]
  ),
  {_, Time} = measure(Function),
  measure_(Function, Result + Time, min(Best, Time), max(Worst, Time), Amount - 1).

measure(Function) ->
  Start = now(),
  FunctionResult = Function(),
  End = now(),
  Time = timer:now_diff(End, Start),
  print(?MEASURE_FILE_NAME,
    ["Measurement result: ", FunctionResult, " after ", Time]
  ),
  {FunctionResult, Time}.
