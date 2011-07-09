-module(game).
-include_lib("nitrogen_core/include/wf.hrl").
-compile(export_all).

start(Gamename,Username) ->
	

join(ID) ->
	wf:comet(fun() -> loop() end).

loop() ->
	receive
		'INIT' ->
			wf:flush();
		{message,User,Text} ->
			Text	
