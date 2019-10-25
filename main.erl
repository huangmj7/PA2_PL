-module(main).
-export([start/0,siteCreator/3,hmSite/6,checkRoute/4,isSame/2,clientPrint/0]).
%-import(util,[hash/2,getRandomNode/0]).
-import(math,[pow/2]).
-import(timer,[sleep/1]).
%-compile(export_all).

% NOTE: Any function called with spawn/ MUST be exported
% You can do this by listing them explicitly in -export([foo/1])
% of with -compile(export_all), which'll give you a compilation warning
% You also need to export a function to create an explicit reference to
% it for higher-order programming.

%Adjustments on handlerrequest().
%They will send requests to reuqired actors.
%Some ILLEGAL inputs, such as required node/actor name > the total number of nodes lead to direct program crash
%SUCH AS: insert 8 testing foo with Nodes = 4 (N = 2)
handleRequest(insert) -> 
	{ok, [Origin, Key, Value]} = io:fread("", "~d ~s ~s"),
	%io:format("inserting key=~s value=~s origin=~b~n", [Key,Value,Origin]),
	FromNode = list_to_atom(integer_to_list(Origin)),
	global:whereis_name(FromNode) ! {insert,Key,Value};
handleRequest(query) ->
	{ok, [QID, Origin, Key]} = io:fread("", "~d ~d ~s"),
	%io:format("querying key=~s starting at node=~b ID=~b~n", [Key, Origin, QID]),
	FromNode = list_to_atom(integer_to_list(Origin)),
	global:whereis_name(FromNode) ! {query,Key,QID,Origin};
handleRequest(stop) -> self() ! finish;
handleRequest(Request) -> io:format("ERROR: Illegal request ~p~n", [Request]).

%Recusive program to receive and handle requests.
%This program will run forever
%Unless it is externally stopped (i.e: Ctrl C)
processRequests() ->
	{ok, [RequestString]} = io:fread("", "~s"),
	Request = list_to_atom(RequestString),
	handleRequest(Request),
	processRequests().

%The client actor which print out message from query
%if applied a wrong key, message will be sent to client but no result will be print out.
clientPrint() ->
	receive 
		{K,{ok,V},N,{A,I}} ->
			io:format("Request ~b sent to agent ~b: Value for key ~s stored in node ~b: ~s~n",[A,I,K,N,V]),
			clientPrint()
	after 
		0 ->
			clientPrint()
	end.

%The inital creator of all actors
%It register the name [0..pow(2,N)-1] for each node globally
siteCreator(-1,Num,Address) -> true;%io:fwrite(" ~b done~n",[Num]);
siteCreator(N,Num,Address) -> 
	%io:fwrite("~b ",[N]),
	UAN = util:getRandomNode(),
	Site = spawn(UAN,main,hmSite,[N,maps:from_list([]),Num,false,Address,[]]),
	Name = list_to_atom(integer_to_list(N)),
	global:register_name(Name,Site),
	siteCreator(N-1,Num,Address).
%Helper function: no longer in use
isSame(I1,A2) -> list_to_atom(integer_to_list(I1)) == A2.

%The actor 
%Name: the number(0 to 2^N-1)
%KeyValue: map hold all key and value 
%N:2^N nodes
%Loc: address of client
%Ready: true/false show if key or value being added to the node
%IDs: list of tuples {QID,InitialNodeRequested}, for wait case of query
hmSite(Name,KeyValue,N,Ready,Loc,IDs)->

	receive
		%Insert case
		{insert,K,V} ->
		        Nodes = round(pow(2,N)),	
			Dest = util:hash(K,Nodes),
			if Dest == Name ->                                     %Insert
				   NewKeyValue = maps:put(K,V,KeyValue),
				   hmSite(Name,NewKeyValue,N,true,Loc,IDs);
			   true ->                                             %Looking for right node
				   %io:format("K is ~s V is ~s H is ~b N is ~b~n",[K,V,hash(K,N),Name]),
				   NewDest = checkRoute(Name,Dest,0,N),
				   NewDestName = list_to_atom(integer_to_list(NewDest)),
				   global:whereis_name(NewDestName) ! {insert,K,V},
				   %io:format("sending to ~b~n",[NewDest]),
				   hmSite(Name,KeyValue,N,Ready,Loc,IDs)
			end;
		%Query case 
		{query,K,A,Init} ->
			Nodes = round(pow(2,N)),
			Dest = util:hash(K,Nodes),
			
			if Dest == Name->                                    %Querying
				   if Ready ->                               
					      %io:format("In ~b to Agent ~b is ~w~n",[Name,A,Ready]),
					      Key = K,
					      Value = maps:find(Key,KeyValue),
					      %io:format("Find: ~s~n",Value),
					      if not (Value == error) ->    %Sending to client
							 Loc ! {Key,Value,Name,{A,Init}},
							 hmSite(Name,KeyValue,N,Ready,Loc,IDs);
						 true ->                    %Wait case1: no key yet
							 NewIDs = IDs ++ [{A,Init,K}],
							 hmSite(Name,KeyValue,N,Ready,Loc,NewIDs)
					      end;
				      not Ready ->                           %Wait case2: no key at all
					      NewIDs = IDs ++ [{A,Init,K}],
					      %io:format("Wait ~b~n",[Name]),
					      hmSite(Name,KeyValue,N,Ready,Loc,NewIDs)
				   end;
			   true ->                                      %find the right actor
				   NewDest = checkRoute(Name,Dest,0,N),
				   NewDestName = list_to_atom(integer_to_list(NewDest)),
				   global:whereis_name(NewDestName) ! {query,K,A,Init},
				   %io:format("~b ask other ~b~n",[Name,NewDest]),
				   hmSite(Name,KeyValue,N,Ready,Loc,IDs)
			end;

		stop -> io:format("Stopped~n")             %ignore case for testing use only

	after 
		0 ->
			if Ready ->                         %Sending out these message that is ready
				   [Loc ! {K,maps:find(K,KeyValue),Name,{A,I}}|| {A,I,K} <- IDs], %crash if invalid key applied, which should never happen
				   RemineList = [{A,I,K} || {A,I,K}<-IDs,maps:find(K,KeyValue) == error],
				   hmSite(Name,KeyValue,N,Ready,Loc,RemineList);
			   true ->                           %wait for first insert
				   hmSite(Name,KeyValue,N,Ready,Loc,IDs)
			end	
	end.

%Helper function to determine the next actor sending 
checkRoute(From,Dest,N,Num) -> 
	Guess = (round(pow(2,N)) + From)  rem round(pow(2,Num)),
	NextGuess = (round(pow(2,N+1)) + From) rem round(pow(2,Num)),
	D1 = abs(Dest - Guess),
	D2 = abs(Dest - NextGuess),
	%io:format("L: ~b G: ~b N:~b~n",[Dest,Guess,N]),
	if 
		N >= (Num-1) -> Guess;       %longest path
		Guess == Dest -> Guess;  %found
		Dest < NextGuess-> Guess; %cloest forward guess
		D1 < D2 -> Guess; %fulfillment
		
		true -> checkRoute(From,Dest,N+1,Num)
	end.

start() ->
	{ok, [N]} = io:fread("", "~d"),
	Nodes = round(math:pow(2, N)),
	ClientAgent = spawn(node(),main,clientPrint,[]), %register to local
	%io:format("With ~b nodes, key \"testing\" belongs in node ~b  in Node ~p~n \n", [Nodes, util:hash("testing", Nodes),node()]),
        global:register_name(client, ClientAgent),
        siteCreator(Nodes-1,N,ClientAgent),
        processRequests().
