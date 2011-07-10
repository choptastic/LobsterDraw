-module(game_server).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-behaviour(gen_server).
-export([start/2,start_link/2,stop/0,init/1,handle_call/3,terminate/2]).
-export([join/2,queue/2,leave/1,guess/2,ready/1,unready/1,title/1,name/1]).



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

terminate(Reason,_State) ->
	{ok, Reason}.

handle_call(stop,_From,Game) ->
	{stop,ok,Game};

handle_call(title,_From,Game) ->
	{reply,Game#game.name,Game};

handle_call({join,Playername},{FromPid,_},Game) ->
	Player = #player{
		name=Playername,
		pid=FromPid
	},
	NewGame = Game#game{
		players=[{FromPid,Player} | Game#game.players]
	},
	to_all_players(NewGame,{join,Playername}),
	{reply,ok,NewGame};

handle_call(leave,{FromPid,_},Game) ->
	Player = pl:get(Game#game.players,FromPid),
	Playername = Player#player.name,

	NewGame = Game#game{
		players=pl:delete(Game#game.players,FromPid)
	},

	to_all_players(NewGame,{leave,Playername}),
	{reply,ok,NewGame};

handle_call({guess,Text},{FromPid,_},Game) ->
	NewG = string:to_lower(Text),
	Target = Game#game.word,
	case NewG of
		Target -> 
			Player = pl:get(Game#game.players,FromPid),
			to_all_players(Game,{correct,Player#player.name,10}),
			NewPlayer = Player#player{
				score=Player#player.score + 10,
				correct=true
			},
			NewGame = Game#game{
				players=pl:set(Game#game.players,FromPid,NewPlayer)
			},
			{reply,ok,NewGame};
		_ ->
			{reply,ok,Game}
	end;
			
handle_call(playerlist,_From,Game) ->
	Players = [P || {_,P} <- Game#game.players],
	{reply,Players,Game};

handle_call({queue,ActionList},{FromPid,_},Game) ->
	to_all_players_except(Game,FromPid,{queue,ActionList}),
	{reply,ok,Game};

handle_call({ready,TF},{FromPid,_},Game) ->
	Player = pl:get(Game#game.players,FromPid),
	NewPlayer = Player#player{ready=TF},
	NewGame = Game#game{
		players=pl:set(Game#game.players,FromPid,NewPlayer)
	},
	
	Msg = case TF of
		true ->
			{ready,Player#player.name};
		false ->
			{unready,Player#player.name}
	end,

	to_all_players(NewGame,Msg),

	case should_we_start(NewGame) of
		true ->
			StartGame = NewGame#game{
				words=word_server:get(20),
				going=true,

			},
			
			{_,NextPlayer} = hd(NewGame#game.players),
			Playername = NextPlayer#player.name,

			to_all_players(NewGame,{get_ready,Playername}),

			timer:apply_after(5000,?MODULE,new_round,[self()]),

			{reply,ok,StartGame};
		false ->
			{reply,ok,NewGame}
	end;


handle_call(new_round,_From,Game) ->
	[{NextPid,NextPlayer} | Players] = Game#players,
	[Word | Remaining] = Game#words,

	to_player(NextPid,{you_are_up,Word}),
	to_all_players_except(Game,NextPid,{new_round,NextPlayer#player.name}),

	NewPlayers = Players ++ [{NextPid,NextPlayer}],

	{ok,Tref} = timer:apply_after(60000,?MODULE,time_up,[]),

	NewGame = Game#game{
		players=NewPlayers,
		words=Remaining,
		drawing_pid = NextPid,
		word=Word,
		timer_refs = [Tref]
	},

	{reply,ok,NewGame};

handle_call(time_up,_From,Game) ->
	to_all_players(Game,{time_up,Game#game.word}),
	NewGame = Game#game{
		drawing_pid=undefined
	},
	{reply,ok,NewGame};


handle_call(Msg,_,Game) ->
	{reply,{error,unexpected_msg,Msg},Game}.

%% Simplifying the gen_server:call function to support for Pid and ID

game_call(Pid,Msg) when is_pid(Pid) ->
	gen_server:call(Pid,Msg);
game_call(ID,Msg) when is_integer(ID) ->
	Pid = game_master:get_pid(ID),
	game_call(Pid,Msg).





%% Sending Messages back to players

to_all_players(Game,Msg) ->
	to_player(all_player_pids(Game),Msg).

to_all_players_except(Game,ExceptPid,Msg) ->
	to_player(all_player_pids_except(Game,ExceptPid),Msg).

all_player_pids(Game) ->
	[Pid || {Pid,_} <- Game#game.players].

all_player_pids_except(Game,ExceptPid) ->
	lists:delete(ExceptPid,all_player_pids(Game)).
	

to_player(Pid,Msg) when is_pid(Pid) ->
	Pid ! Msg,
	ok;
to_player(Pids,Msg) when is_list(Pids)->
	[to_player(Pid,Msg) || Pid <- Pids],
	ok.
	

%% DrawAction is formatted based on what is sent from the javascript.
%% It doesn't matter to us, since we're just redistributing it to the clients
queue(Pid,ActionList) ->
	game_call(Pid,{queue,ActionList}).

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

new_round(Pid) ->
	game_call(Pid,new_round).

%% Private functions

should_we_start(Game) ->
	Total = length(Game#game.players),
	Ready = length([P || {_,P} <- Game#game.players,P#player.ready==true]),
	Ready > Total/2.
	
