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

%% API
-compile(export_all).

print(List) -> util:logging("Log", lists:concat([List, "\n"])).

measure(F) ->
  B = now(),
  V = F(),
  A = now(),
  {timer:now_diff(A,B), V}.