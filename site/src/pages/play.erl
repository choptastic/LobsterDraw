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
	"<canvas class=game_canvas width=640 height=480>Get a browser that doesn't suck</canvas>";
	

playerlist() ->
	[].


clock() ->
	[].


controls() ->
	[].


start_drawing() ->
	

event(guess) ->
	GuessText = wf:q(guess);
event(_) -> 
	ok.

api_event(api_draw,_,[line,Color,X1,Y1,X2,Y2]) ->
	ok;
api_event(api_draw,_,[erase]) ->
	ok;
api_event(api_draw,_,[fill,Color,X,Y]) ->
	ok.
