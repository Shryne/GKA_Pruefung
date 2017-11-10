%%%-------------------------------------------------------------------
%%% @author Steven
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2017 09:47
%% TODO: dot prints an error for graphs with more than 4 vertices

%%%-------------------------------------------------------------------
-module(test_creation).
-author("Steven").

-define(CUSTOM_GRAPHS_PATH, "eigene_graphen/").
-define(FILE_NAME_BASE, "graph_").
-define(OUTPUT_FILE_TYPE, ".graph").
-define(TO_PICTURE_MAX_COMPLEXITY, 100).
-define(DOT_COMMAND_START, "\"C:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe\" -Tpng ").
-define(PICTURE_TYPE, ".png").
-define(DOT_TYPE, ".dot").

%% API
-export([start/1]).

start(VertexAmount) ->
  filelib:ensure_dir(?CUSTOM_GRAPHS_PATH),
  GraphName = lists:append(?FILE_NAME_BASE, util:to_String(VertexAmount)),
  GraphPath = lists:append([?CUSTOM_GRAPHS_PATH, GraphName, ?OUTPUT_FILE_TYPE]),
  file:delete(GraphPath),
  gengraph:gengraph(VertexAmount, VertexAmount * 2, 1, VertexAmount, GraphPath),
  to_picture(GraphName, VertexAmount, VertexAmount * 2).


to_picture(GraphName, VertexAmount, Branching)
  when Branching * VertexAmount =< ?TO_PICTURE_MAX_COMPLEXITY ->

  % the d is not important here, because it doesn't make a difference for the print
  Graph = adtgraph:importG(lists:append([?CUSTOM_GRAPHS_PATH, GraphName, ?OUTPUT_FILE_TYPE]), d),

  DotInputPath = lists:append([?CUSTOM_GRAPHS_PATH, GraphName, ?DOT_TYPE]),
  file:delete(DotInputPath),
  adtgraph:printG(Graph, DotInputPath),
  PicturePath = lists:append([?CUSTOM_GRAPHS_PATH, GraphName, ?PICTURE_TYPE]),
  file:delete(PicturePath),
  io:fwrite(lists:append([?DOT_COMMAND_START, DotInputPath, " > ", PicturePath])),
  os:cmd(lists:append([?DOT_COMMAND_START, DotInputPath, " > ", PicturePath]));
to_picture(_, _, _) -> to_high_complexity_to_print.