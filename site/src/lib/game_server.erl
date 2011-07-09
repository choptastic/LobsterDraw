-module(game_server).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(gen_server).
-export([start/0,start_link/0,stop/0,init/1,handle_call/3,handle_cast/3,terminate/2]).
-export([list/0,new/1,join/1,guess/1,draw/2]).

start() ->
	gen_server:start({local,?MODULE},?MODULE, [], []).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

init([]) ->
	{ok, []}.

handle_call(stop,_From,Games) ->
	{stop,ok,Games};
handle_call({new,Name},From,Games) ->
	ID = crypto:rand_uniform(1,9999999999999999999999999),	%% LOL EXCESSIVE RANDOM
	NewGame = #game{
		id=ID,
		name=Name,
		words=load_words()
	},

	%% Games are stored as a proplist {ID,Game_Record)
	{reply,ID,[{ID,NewGame} | Games]};

handle_call({join,ID,Playername},FromPid,Games) ->
	Game = pl:get(Games,ID),
	Player = #player{
		name=Playername,
		pid=FromPid
	},
	NewGame = Game#game{
		players=[{FromPid,Player} | Game#game.players]
	},
	{reply,ok,pl:set(Games,ID,NewGame)};

handle_call({leave,ID},FromPid,Games) ->
	Game = pl:get(Games,ID),
	NewGame = Game#game{
		players=pl:delete(Game#game.players,FromPid)
	},
	{reply,ok,pl:set(Games,ID,NewGame)};

handle_call({guess,ID,text}) ->
	:

handle_call(_,_,Games) ->
	Games.
	
	



load_words() ->
	["dog","bear","cat","apple","france"].
