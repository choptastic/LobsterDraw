-module(game).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

start(Gamename,Username) ->
	ok.
	

join(ID) ->
	wf:comet(fun() -> loop() end).

loop() ->
	ok.

exists(ID) ->
	game_master:exists(ID).


title(ID) ->
	game_master:title(ID).
