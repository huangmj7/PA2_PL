-module(main).
-export([start/0,hashMapSite/3,siteCreator/3,hmSite/7,checkRoute/4,isSame/2,clientPrint/0]).
-import(util,[hash/2,getRandomNode/0]).
-import(math,[pow/2]).
-import(timer,[sleep/1]).
-import(lists,[append/2]).
%-compile(export_all).

% NOTE: Any function called with spawn/ MUST be exported
% You can do this by listing them explicitly in -export([foo/1])
% of with -compile(export_all), which'll give you a compilation warning
% You also need to export a function to create an explicit reference to
% it for higher-order programming.

handleRequest(insert) -> 
	{ok, [Origin, Key, Value]} = io:fread("", "~d ~s ~s"),
	io:format("inserting key=~s value=~s origin=~b~n", [Key,Value,Origin]),
	FromNode = list_to_atom(integer_to_list(Origin)),
	FromNode ! {insert,Key,Value};
handleRequest(query) ->
	{ok, [QID, Origin, Key]} = io:fread("", "~d ~d ~s"),
	io:format("querying key=~s starting at node=~b ID=~b~n", [Key, Origin, QID]),
	FromNode = list_to_atom(integer_to_list(Origin)),
	FromNode ! {query,Key,QID,Origin};
handleRequest(stop) -> self() ! finish;
handleRequest(Request) -> io:format("ERROR: Illegal request ~p~n", [Request]).
processRequests() ->
	{ok, [RequestString]} = io:fread("", "~s"),
	Request = list_to_atom(RequestString),
	handleRequest(Request),
	processRequests().

%	receive
%		{K,V,From,{ID,Init}} ->
%			io:format("Request ~b send to node ~b has K ~s and V ~s in ~b~n",[ID,From,K,V,Init]),
%			processRequests()
%	after 
%		0 ->
%			%{ok,[RequestString]} = io:fread("","~s"),
%			%Request = list_to_atom(RequestString),
%			handleRequest(Request),
%			processRequests()
%	end.

clientPrint() ->
	receive 
		{K,V,N,{A,I}} ->
			io:format("Request ~b sent to agent ~b: Value for key ~s stored in node ~b: ~s~n",[A,I,K,N,V]),
			clientPrint()
	after 
		0 ->
			clientPrint()
	end.

hashMapSite(Key,Value,Node) ->
	io:fwrite("key is ~s and value is ~s and the hashkey is ~b and the pid is ~p~n",[Key,Value,util:hash(Key,Node),self()]).

siteCreator(-1,Num,Address) -> true;%io:fwrite(" ~b done~n",[Num]);
siteCreator(N,Num,Address) -> 
	%io:fwrite("~b ",[N]),
	Site = spawn(main,hmSite,[N,"_null_","_null_",Num,false,Address,[]]),
	Name = list_to_atom(integer_to_list(N)),
	register(Name,Site),
	siteCreator(N-1,Num,Address).

isSame(I1,A2) -> list_to_atom(integer_to_list(I1)) == A2.
hmSite(Name,Key,Value,N,Ready,Loc,IDs)->

	receive
		{K,V} -> hmSite(Name,K,V,N,Ready,Loc,IDs);
		{insert,K,V} -> 
			Dest = util:hash(K,N),
			if Dest == Name -> hmSite(Name,K,V,N,true,Loc,IDs);
			   true ->
				   %io:format("K is ~s V is ~s H is ~b N is ~b~n",[K,V,hash(K,N),Name]),
				   NewDest = checkRoute(Name,Dest,0,N),
				   NewDestName = list_to_atom(integer_to_list(NewDest)),
				   NewDestName ! {insert,K,V},
				   %io:format("sending to ~b~n",[NewDest]),
				   hmSite(Name,Key,Value,N,Ready,Loc,IDs)
			end;
		{query,K,A,Init} ->
			Dest = util:hash(K,N),
			io:format("To ~b~n",[Dest]),
			if Dest == Name->
				   if Ready -> 
					      %io:format("In ~b K is ~s V is ~s to Agent ~b is ~w~n",[Name,Key,Value,A,Ready]),
					      Loc ! {Key,Value,Name,{A,Init}},
					      hmSite(Name,Key,Value,N,Ready,Loc,IDs);
				      not Ready -> 
					      NewIDs = IDs ++ [{A,Init}],
					     %io:format("Wait ~b~n",[Name]),
					      hmSite(Name,Key,Value,N,Ready,Loc,NewIDs)
				   end;
			   true ->
				   NewDest = checkRoute(Name,Dest,0,N),
				   NewDestName = list_to_atom(integer_to_list(NewDest)),
				   NewDestName ! {query,K,A,Init},
				   %io:format("~b ask other ~b~n",[Name,NewDest]),
				   hmSite(Name,Key,Value,N,Ready,Loc,IDs)
			end;

		stop -> io:format("V:~s K:~s ~n",[Key,Value])

	after 
		0 ->
			if Ready ->
				   [ Loc ! {Key,Value,Name,X} || X <- IDs],
				   hmSite(Name,Key,Value,N,Ready,Loc,[]);
			   true -> 
				   hmSite(Name,Key,Value,N,Ready,Loc,IDs)
			end	
	end.


%Assume N < given N and N initally is 0
%need include math module
checkRoute(From,Dest,N,Num) -> 
	Guess = (round(pow(2,N)) + From)  rem round(pow(2,Num)),
	NextGuess = (round(pow(2,N+1)) + From) rem round(pow(2,Num)),
	%io:format("L: ~b G: ~b N:~b~n",[Dest,Guess,N]),
	if 
		N >= (Num-1) -> Guess;       %longest path
		Guess == Dest -> Guess;  %found
		Dest < NextGuess-> Guess; %closet path
		
		true -> checkRoute(From,Dest,N+1,Num)
	end.

start() ->
	{ok, [N]} = io:fread("", "~d"),
	Nodes = round(math:pow(2, N)),
	io:format("With ~b nodes, key \"cat\" belongs in node ~b  in Node~p~n \n", [Nodes, util:hash("cat", Nodes),node()]),
	%io:fromat("Host: ~w~n",[nodes()]),
	%ClientAgent = spawn(main,clientPrint,[]),
	%register(client, ClientAgent),
	%siteCreator(Nodes-1,N,ClientAgent),
	%processRequests().
