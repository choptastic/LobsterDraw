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



%% DrawAction is formatted based on what is sent from the javascript.
%% It doesn't matter to us, since we're just redistributing it to the clients
draw(ID,DrawAction) ->
	ok.

%% Private functions

load_words() ->
	["dog","bear","cat","apple","france"].
