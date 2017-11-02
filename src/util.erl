-module(util).
-compile(export_all).
-define(MILL, 1000000).
-define(ZERO, integer_to_list(0)).

%% -------------------------------------------
% entfernt Duplikate in der Liste
%
list2set([])    -> [];
list2set([H|T]) -> [H | [X || X <- list2set(T), X /= H]].	

%% Wandelt Liste in eine Zeichenketten Liste um
list2string([]) -> "\n";
list2string([H|T]) -> lists:concat([H," ",list2string(T)]).

% Schreibt eine Liste in eine Datei
writelist(List,Filename) -> file:write_file(Filename,io_lib:format("~w",[List])).
% Liest eine solche Liste aus einer Datei
readlist(Filename) -> {Sign,ListBinary} = file:read_file(Filename),
                        case Sign of
					       ok -> slist_tointlist(string:tokens(binary_to_list(ListBinary),"[],")); 
						   error -> io:format("in readlist: Fehler beim Lesen von ~p: ~p\n",[Sign,ListBinary])
					    end.
% transformiert die Liste von Zeichenketten in eine Liste von Integer						
slist_tointlist([]) -> [];
slist_tointlist([SInt|STail]) -> 
        {IntS,Case} = string:to_integer(SInt), 
        case IntS of
			error -> io:format("in slist_tointlist: Fehler beim transformieren: ~p\n",[Case]);
            _Any -> [IntS|slist_tointlist(STail)] 
		end.
	
%% -------------------------------------------
% Erzeugt eine sortierte Liste mit Num Zahlen
%
sortliste(Num) ->
	lists:seq(1, Num).
% Erzeugt eine umgekehrt sortierte Liste mit Num Zahlen 
resortliste(Num) ->
	lists:reverse(lists:seq(1, Num)).
% Erzeugt eine unsortierte Liste mit Num Zufallszahlen im Bereich 1 Num
% ohne Duplikate 
randomliste(Num) ->
    shuffle([X || X <- lists:seq(1, Num)]).
% Erzeugt eine unsortierte Liste mit Num Zufallszahlen im Bereich Min Max
% Duplikate sind möglich 
randomlisteD(Num,Min,Max) ->
	RangeInt = Max-Min,
	lists:flatten([rand:uniform(RangeInt+1) + Min-1 || _ <- lists:seq(1, Num)]).
	
%% -------------------------------------------
%% Mischt eine Liste
% Beispielaufruf: NeueListe = shuffle([a,b,c]),
%
%shuffle(List) -> shuffle(List, []).
%shuffle([], Acc) -> Acc;
%shuffle(List, Acc) ->
%    {Leading, [H | T]} = lists:split(rand:uniform(length(List)) - 1, List),
%    shuffle(Leading ++ T, [H | Acc]).
shuffle(List) -> PList = [{rand:uniform(),Elem} || Elem <- List],
                 [Elem || {_,Elem}<- lists:keysort(1,PList)].

	
%% -------------------------------------------
%% Transformiert float nach int
% gerundet wird nach unten 3.3 und 3.8 ergeben 3
%
float_to_int(Float) -> list_to_integer(float_to_list(Float, [{decimals, 0}])).
	
%% -------------------------------------------
% Ermittelt den Typ
% Beispielaufruf: type_is(Something),
%
type_is(Something) ->
    if is_atom(Something) -> atom;
	   is_binary(Something) -> binary;
	   is_bitstring(Something) -> bitstring;
	   is_boolean(Something) -> boolean;
	   is_float(Something) -> float;
	   is_function(Something) -> function;
	   is_integer(Something) -> integer;
	   is_list(Something) -> list;
	   is_number(Something) -> number;
	   is_pid(Something) -> pid;
	   is_port(Something) -> port;
	   is_reference(Something) -> reference;
	   is_tuple(Something) -> tuple
	end.
	
% Wandelt in eine Zeichenkette um
% Beispielaufruf: to_String(Something),
%
to_String(Etwas) ->
	lists:flatten(io_lib:format("~p", [Etwas])).	
	
%% -------------------------------------------
% Ein globaler Zähler
%
counting1(Counter) -> Known = erlang:whereis(Counter),
						 case Known of
							undefined -> PIDcountklc = spawn(fun() -> countloop(0) end),
										 erlang:register(Counter,PIDcountklc);
							_NotUndef -> ok
						 end,
						 Counter ! {count,1},
						 ok.

counting(Counter,Step) -> Known = erlang:whereis(Counter),
						 case Known of
							undefined -> PIDcountklc = spawn(fun() -> countloop(0) end),
										 erlang:register(Counter,PIDcountklc);
							_NotUndef -> ok
						 end,
						 Counter ! {count,Step},
						 ok.

countread(Counter) -> Known = erlang:whereis(Counter),
						case Known of
							undefined -> 0;
							_NotUndef -> 
								Counter ! {get,self()},
								receive
									{current,Num} -> Num;
									_SomethingElse -> 0
								end
						end.

countreset(Counter) -> 	Known = erlang:whereis(Counter),
				case Known of
					undefined -> false;
					_NotUndef -> Counter ! reset, true
				end.

countstop(Counter) -> 	Known = erlang:whereis(Counter),
				case Known of
					undefined -> false;
					_NotUndef -> Counter ! kill, true
				end.
					
countloop(Count) -> receive
						{count,Num} -> countloop(Count+Num);
						{get,PID} -> PID ! {current,Count},
									countloop(Count);
						reset -> countloop(0);
						kill -> true
					end.
%% -------------------------------------------
% Eine globale Variable
%
globalvar(VariableName) -> Known = erlang:whereis(VariableName),
						 case Known of
							undefined -> PIDcountklc = spawn(fun() -> glvarloop(nil) end),
										 erlang:register(VariableName,PIDcountklc);
							_NotUndef -> ok
						 end,
						 ok.

setglobalvar(VariableName,Value) -> Known = erlang:whereis(VariableName),
						 case Known of
							undefined -> PIDcountklc = spawn(fun() -> glvarloop(nil) end),
										 erlang:register(VariableName,PIDcountklc);
							_NotUndef -> ok
						 end,
						 VariableName ! {writevar,Value},
						 ok.

getglobalvar(VariableName) -> Known = erlang:whereis(VariableName),
						case Known of
							undefined -> nil;
							_NotUndef -> 
								VariableName ! {get,self()},
								receive
									{current,Value} -> Value;
									_SomethingElse -> nil
								end
						end.

globalvarreset(VariableName) -> 	Known = erlang:whereis(VariableName),
				case Known of
					undefined -> false;
					_NotUndef -> VariableName ! reset, true
				end.

globalvarstop(VariableName) -> 	Known = erlang:whereis(VariableName),
				case Known of
					undefined -> false;
					_NotUndef -> VariableName ! kill, true
				end.
					
glvarloop(Value) -> receive
						{writevar,NewValue} -> glvarloop(NewValue);
						{get,PID} -> PID ! {current,Value},
									glvarloop(Value);
						reset -> glvarloop(nil);
						kill -> true
					end.
					
%% -------------------------------------------
% Schreibt auf den Bildschirm und in eine Datei
% nebenläufig zur Beschleunigung
% Beispielaufruf: logging('FileName.log',"Textinhalt"),
%
% logging(_Datei,_Inhalt) -> ok;
logging(Datei,Inhalt) -> Known = erlang:whereis(logklc),
						 case Known of
							undefined -> PIDlogklc = spawn(fun() -> logloop(0) end),
										 erlang:register(logklc,PIDlogklc);
							_NotUndef -> ok
						 end,
						 logklc ! {Datei,Inhalt},
						 ok.

logstop( ) -> 	Known = erlang:whereis(logklc),
				case Known of
					undefined -> false;
					_NotUndef -> logklc ! kill, true
				end.
					
logloop(Y) -> 	receive
					{Datei,Inhalt} -> %io:format(Inhalt),
									  file:write_file(Datei,Inhalt,[append]),
									  logloop(Y+1);
					kill -> true
				end.

%% Zeitstempel: 'MM.DD HH:MM:SS,SSS'
% Beispielaufruf: Text = lists:concat([Clientname," Startzeit: ",timeMilliSecond()]),
%
timeMilliSecond() ->
	{_Year, Month, Day} = date(),
	{Hour, Minute, Second} = time(),
	Tag = lists:concat([klebe(Day,""),".",klebe(Month,"")," ",klebe(Hour,""),":"]),
	{_, _, MicroSecs} = erlang:timestamp(),
	Tag ++ concat([Minute,Second],":") ++ "," ++ toMilliSeconds(MicroSecs)++"|".
toMilliSeconds(MicroSecs) ->
	Seconds = MicroSecs / ?MILL,
	%% Korrektur, da string:substr( float_to_list(0.234567), 3, 3). 345 ergibt
	if (Seconds < 1) -> CorSeconds = Seconds + 1;
	   (Seconds >= 1) -> CorSeconds = Seconds
	end,
	string:substr( float_to_list(CorSeconds), 3, 3).
concat(List, Between) -> concat(List, Between, "").
concat([], _, Text) -> Text;
concat([First|[]], _, Text) ->
	concat([],"",klebe(First,Text));
concat([First|List], Between, Text) ->
	concat(List, Between, string:concat(klebe(First,Text), Between)).
klebe(First,Text) -> 	
	NumberList = integer_to_list(First),
	string:concat(Text,minTwo(NumberList)).	
minTwo(List) ->
	case {length(List)} of
		{0} -> ?ZERO ++ ?ZERO;
		{1} -> ?ZERO ++ List;
		_ -> List
	end.


