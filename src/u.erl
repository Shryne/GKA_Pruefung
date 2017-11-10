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

log(Name, List) -> util:logging(Name, lists:flatten([List, "\n"])).

% Function to measure the time needed to complete the given function.
% Use: measure(fun() -> ... end[, number]).
measure(FileName, Function) -> measure(FileName, Function, 1).
measure(FileName, Function, Amount) ->
  file:delete(FileName),
  {FResult, All, Best, Worst} = measure_(FileName, Function, nothing, 0, inf, 0, Amount),
  log(FileName,
    ["Measurements done. Avg: ", util:to_String(All / Amount), ", Best: ", util:to_String(Best), ", Worst: ", util:to_String(Worst)]
  ),
  FResult.

measure_(_, _, FResult, Result, Best, Worst, 0) -> {FResult, Result, Best, Worst};
measure_(FileName, Function, _, Result, Best, Worst, Amount) ->
  log(FileName,
    ["Measurement number ", util:to_String(Amount), " starting..."]
  ),
  Start = now(),
  FResult = Function(),
  Time = timer:now_diff(now(), Start),
  measure_(FileName, Function, FResult, Result + Time, min(Best, Time), max(Worst, Time), Amount - 1).

% Like util:list2string, but without the "\n" character at the end.
% I removed the list from the name, because that's the type of the parameter anyway and that's nothing hidden.
toString([]) -> "";
toString([H|T]) -> lists:concat([H, " ", toString(T)]).