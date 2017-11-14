%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2017 08:33
%%% To run the tests, compile this module (c(dijkstra_test).) and run dijkstra_test:test().
%%%-------------------------------------------------------------------
-module(test).
-author("Steven").

-include_lib("eunit/include/eunit.hrl").


-define(TEST_MODULE_LIST,
  [
    %fun(FileName, StartVertex, Variant) -> dijkstra:dijkstra(FileName, StartVertex, Variant) end
    fun(FileName, StartVertex, Variant) -> dijkstra3:dijkstra(FileName, StartVertex, Variant) end,
    fun(FileName, StartVertex, Variant) -> bellmannford:bellmannford(FileName, StartVertex, Variant) end
  ]
).


both_test_() ->
  lists:map(
    fun(Function) ->
      [
        ?_assertEqual([], Function("BLA", 0, d)),
        ?_assertEqual(
          lists:sort([{11, 0, 11}, {22, 8, 33}, {33, 5, 11}, {44, 9, 22}, {55, 7, 33}]),
          lists:sort(Function("graphen/graph_03.graph", 11, x))),
        ?_assert(
          % Because there are two ways to 55 with a cost of 7 in the undirected version of graph_03.
          (lists:sort([{11, 0, 11}, {22, 8, 33}, {33, 5, 11}, {44, 9, 22}, {55, 7, 33}]) ==
            lists:sort(Function("graphen/graph_03.graph", 11, ud))) or
            (lists:sort([{11, 0, 11}, {22, 8, 33}, {33, 5, 11}, {44, 9, 22}, {55, 7, 11}]) ==
              lists:sort(Function("graphen/graph_03.graph", 11, ud)))
        ),
        ?_assertEqual(
          lists:sort(
            [{18119, 0, 18119}, {23569, 125, 18119}, {12099, 222, 18119}, {20099, 190, 23569}, {28199, 313, 20099},
              {30159, 340, 20099}, {49090, 437, 28199}, {4299, 410, 12099}, {33699, 490, 49090}, {44149, 550, 28199},
              {34119, 578, 30159}, {50999, 647, 44149}, {60599, 768, 34119}, {1099, 436, 12099}, {66119, 930, 50999},
              {55129, 811, 60599}, {90449, 761, 1099}, {67659, 891, 55129}, {68199, 874, 60599}, {76199, 932, 68199},
              {70199, 1013, 76199}, {86199, 1162, 70199}, {80999, 923, 90449}]),
            lists:sort(Function("graphen/d_graph_de.graph", 18119, d))
        ),
        ?_assertEqual(
          lists:sort(
            [{18119, 0, 18119}, {23569, 125, 18119}, {12099, 222, 18119}, {20099, 190, 23569}, {28199, 313, 20099},
              {30159, 340, 20099}, {49090, 437, 28199}, {4299, 410, 12099}, {33699, 490, 49090}, {44149, 550, 28199},
              {34119, 578, 30159}, {50999, 647, 44149}, {60599, 768, 34119}, {1099, 436, 12099}, {66119, 930, 50999},
              {55129, 811, 60599}, {90449, 761, 1099}, {67659, 891, 55129}, {68199, 874, 60599}, {76199, 932, 68199},
              {70199, 1013, 76199}, {86199, 1004, 80999}, {80999, 923, 90449}]),
          lists:sort(Function("graphen/ud_graph_de.graph", 18119, ud))
        )
      ] end,
    ?TEST_MODULE_LIST
  ).

% Some results are different, because dijkstra can't deal with negative costs
dijkstra_test_() ->
  [
    ?_assertEqual(
      % Because there are two ways to 55 with a cost of 7 in the undirected version of graph_03.
      lists:sort([{11, 0, 11}, {12, 1, 11}, {13, 10, 11}, {14, 10, 11}, {15, 20, 13}, {16, 20, 14}, {17, 30, 16},
        {18, 2, 12}, {19, 12, 18}]),
      lists:sort(dijkstra3:dijkstra("graphen/graph_04.graph", 11, d))
    )
  ].

bellmannford_test_() ->
  [
    ?_assertEqual(
      % Because there are two ways to 55 with a cost of 7 in the undirected version of graph_03.
      lists:sort([{11, 0, 11}, {12, 1, 11}, {13, 10, 11}, {14, 10, 11}, {15, 20, 13}, {16, 20, 14}, {17, 30, 16},
        {18, -170, 17}, {19, -160, 18}]),
      lists:sort(bellmannford:bellmannford("graphen/graph_04.graph", 11, d))
    )
  ].