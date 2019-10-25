-module(test).
-export([start/0,express/0]).
-import(util,[hash/2]).

express() ->
        {ok, [N]} = io:fread("Number: ", "~d"),	
	Nodes = round(math:pow(2, N)),
	{ok, [S]} = io:fread("K: ", "~s"),
	io:format("To: ~b~n", [util:hash(S,N)])
	if S == "F" -> ok;
	   true -> express()
	end.

start()->
	express().

