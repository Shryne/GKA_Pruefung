%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2017 08:33
%%% To run the tests, compile this module (c(dijkstra_test).) and run dijkstra_test:test().
%%%-------------------------------------------------------------------
-module(dijkstra_test).
-author("Steven").

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE_LIST, [dijkstra, dijkstra1, dijkstra2]).


illegal_input_test_() ->
  lists:map(
    fun(Module) ->
      [
        ?_assertEqual([], Module:dijkstra("BLA", 0, d)),
        ?_assertEqual(
          lists:sort([{11, 0, 11}, {22, 8, 33}, {33, 5, 11}, {44, 9, 22}, {55, 7, 33}]),
          lists:sort(Module:dijkstra("graphen/graph_03.graph", 0, x))),
        ?_assert(
          % Because there are two ways to 55 with a cost of 7 in the undirected version of graph_03.
          (lists:sort([{11, 0, 11}, {22, 8, 33}, {33, 5, 11}, {44, 9, 22}, {55, 7, 33}]) ==
          lists:sort(Module:dijkstra("graphen/graph_03.graph", 0, ud))) or
            (lists:sort([{11, 0, 11}, {22, 8, 33}, {33, 5, 11}, {44, 9, 22}, {55, 7, 11}]) ==
            lists:sort(Module:dijkstra("graphen/graph_03.graph", 0, ud)))
        )
      ] end,
    ?TEST_MODULE_LIST
  ).