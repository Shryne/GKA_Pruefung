%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2017 23:48
%%% TODO: fix now() depreacation
%%%-------------------------------------------------------------------
-module(u).
-author("Steven").

%% API
-compile(export_all).

log(Name, List) -> util:logging(Name, lists:concat([util:to_String(List), "\n"])).

% Function to measure the time needed to complete the given function.
% Use: measure(fun() -> ... end[, number]).
measure(FileName, Function) -> measure(FileName, Function, 1).
measure(FileName, Function, Amount) ->
  file:delete(FileName),
  {All, Best, Worst} = measure_(FileName, Function, 0, inf, 0, Amount),
  log(FileName,
    ["Measurements done. Avg: ", util:to_String(All / Amount), ", Best: ", util:to_String(Best), ", Worst: ", util:to_String(Worst)]
  ),
  done.

measure_(_, _, Result, Best, Worst, 0) -> {Result, Best, Worst};
measure_(FileName, Function, Result, Best, Worst, Amount) ->
  log(FileName,
    ["Measurement number ", util:to_String(Amount), " starting..."]
  ),
  Start = now(),
  Function(),
  Time = timer:now_diff(now(), Start),
  measure_(FileName, Function, Result + Time, min(Best, Time), max(Worst, Time), Amount - 1).

