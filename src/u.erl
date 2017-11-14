-module(u).
-author("Steven").

%% API
-compile(export_all).

log(Name, List) -> util:logging(Name, lists:flatten([List, "\n"])).

% Like util:list2string, but without the "\n" character at the end.
% I removed the list from the name, because that's the type of the parameter anyway and that's nothing hidden.
toString([]) -> "";
toString([H|T]) -> lists:concat([H, " ", toString(T)]).