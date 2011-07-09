-module(game_server).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(gen_server).
-export([start/2,start_link/2,stop/0,init/1,handle_call/3,terminate/2]).
-export([join/2,draw/2,leave/1,guess/2,ready/1,unready/1,title/1,name/1]).



start(ID,Name) ->
	gen_server:start(?MODULE, {ID,Name}, []).

start_link(ID,Name) ->
	gen_server:start_link(?MODULE, {ID,Name}, []).

init({ID,Name}) ->
	GameRec = #game{
		name=Name,
		pid=self(),
		id=ID
	},
	{ok, GameRec}.

stop() ->
	gen_server:call(?MODULE,stop).

terminate(Reason,State) ->
	{ok, Reason}.

handle_call(stop,_From,Game) ->
	{stop,ok,Game};

handle_call(title,_From,Game) ->
	{reply,Game#game.name,Game};

handle_call({join,Playername},FromPid,Game) ->
	Player = #player{
		name=Playername,
		pid=FromPid
	},
	NewGame = Game#game{
		players=[{FromPid,Player} | Game#game.players]
	},
	{reply,ok,NewGame};

handle_call(leave,FromPid,Game) ->
	NewGame = Game#game{
		players=pl:delete(Game#game.players,FromPid)
	},
	{reply,ok,NewGame};

handle_call({guess,Text},FromPid,Game) ->
	ok;

handle_call({draw,DrawAction},FromPid,Game) ->
	ok;

handle_call({ready,TF},FromPid,Game) ->
	Player = pl:get(FromPid,Game#game.player),
	NewPlayer = Player#player{ready=TF},
	NewGame = Game#game{
		players=pl:set(Game#game.players,NewPlayer)
	},
	{reply,ok,NewGame};

handle_call(next_round,FromPid,Game) ->
	ok;

handle_call(time_up,FromPid,Game) ->
	ok;

handle_call(Msg,_,Game) ->
	{reply,{error,unexpected_msg,Msg},Game}.

game_call(Pid,Msg) when is_pid(Pid) ->
	gen_server:call(Pid,Msg);
game_call(ID,Msg) when is_integer(ID) ->
	Pid = game_master:get_pid(ID),
	game_call(Pid,Msg).

%% DrawAction is formatted based on what is sent from the javascript.
%% It doesn't matter to us, since we're just redistributing it to the clients
draw(Pid,DrawAction) ->
	game_call(Pid,DrawAction).

guess(Pid,Text) ->
	game_call(Pid,{guess,Text}).

join(Pid,Name) ->
	game_call(Pid,{join,Name}).

leave(Pid) ->
	game_call(Pid,leave).

ready(Pid) ->
	game_call(Pid,{ready,true}).

unready(Pid) ->
	game_call(Pid,{ready,false}).

title(Pid) ->
	game_call(Pid,title).

exists(ID) ->
	game_master:exists(ID).

playerlist(Pid) ->
	game_call(Pid,playerlist).

name(Pid) ->
	title(Pid).

%% Private functions
