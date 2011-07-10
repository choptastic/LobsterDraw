-module(word_server).
-export([start/0,start_link/0,init/1,handle_call/3,terminate/2]).
-export([load/1,get/1]).
-behaviour(gen_server).

start() ->
	gen_server:start({local,?MODULE},?MODULE, [], []),
	load("./site/words.txt").

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

init([]) ->
	{ok, []}.

terminate(Reason,State) ->
	{ok,Reason}.

handle_call(stop,_,Queue) ->
	{stop,normal,Queue};

handle_call({load,NewList},From,_Queue) ->
	{reply,ok,queue:from_list(NewList)};

handle_call({get,_Num},From,[]) ->
	{reply,{error,please_load_first},[]};

handle_call({get,Num},From,Queue) ->
	{QGotten,QRest} = queue:split(Num,Queue),
	List = 	queue:to_list(QGotten),
	NewQueue = queue:join(QRest,QGotten), %% put the newly gotten items to the back of the queue
	{reply,List,NewQueue}.

load(Path) ->
	{ok,Data} = file:read_file(Path),
	String = binary_to_list(Data),
	List = string:tokens(String,"\n"),

	%% Slow list randomize sorting, but I'm out of time 
	List2 = lists:sort(fun(_,_) -> 
		crypto:rand_uniform(1,2)==2
	end,List),
		
	gen_server:call(?MODULE,{load,List2}).

get(Num) ->
	gen_server:call(?MODULE,{get,Num}).

