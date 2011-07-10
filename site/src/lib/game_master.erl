-module(game_master).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(gen_server).
-export([start/0,start_link/0,stop/0,init/1,handle_call/3,terminate/2]).
-export([list/0,new/1,get_pid/1]).

start() ->
	gen_server:start({local,?MODULE},?MODULE, [], []).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

init([]) ->
	{ok, []}.

stop() ->
	gen_server:call(?MODULE,stop).

terminate(Reason,State) ->
	{ok, Reason}.

handle_call(stop,_From,Games) ->
	{stop,ok,Games};
handle_call({new,Name},From,Games) ->
	ID = crypto:rand_uniform(1,9999999999999999999999999),	%% LOL EXCESSIVE RANDOM

	{ok,Pid} = game_server:start_link(ID,Name),
	

	%% Games are stored as a list of {ID,Pid,GameName)
	%% Get more info from the game_server module using the pid
	{reply,ID,[{ID,Pid,Name} | Games]};

handle_call({get_pid,ID},From,Games) ->
	%% Don't need to specify a conditional because ID will match
	Pids = [Pid || {GameID,Pid,_} <- Games,GameID==ID],

	MyPid = case Pids of
		[] -> undefined;
		[P] -> P
	end,
	{reply,MyPid,Games};

handle_call(list,From,Games) ->
	List = [{ID,Name} || {ID,_,Name} <- Games],
	{reply,List,Games};		

handle_call(_,_,Games) ->
	Games.
	

%% Interface Functions (those functions used by clients)

new(Name) ->
	gen_server:call(?MODULE,{new,Name}).

get_pid(ID) ->
	gen_server:call(?MODULE,{get_pid,ID}).

exists(ID) ->
	Pid = get_pid(ID),
	is_pid(Pid).


list() ->
	gen_server:call(?MODULE,list).
