-module(play).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

main() ->
	#template{file="./site/templates/game.html"}.
id() ->
	wf:path_info().

title() ->
	[].

canvas() ->
	wf:wire(#api{name=queue,tag=unused}),
	[
	"<canvas class=game_canvas width=640 height=480>Get a browser that doesn't suck</canvas>",
	#br{},
	"<script>enable_drawing()</script>",
	#button{text="Erase",postback=erase}
	].
	

playerlist() ->
	[].


clock() ->
	[].


controls() ->
	[].



event(guess) ->
	GuessText = wf:q(guess);
event(erase) ->
	wf:wire("erase()");
event(_) -> 
	ok.

api_event(queue,_,ActionList) ->
	?PRINT(ActionList).
