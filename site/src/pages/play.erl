-module(play).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

main() ->
	case game:exists(id()) of
		true ->
			#template{file="./site/templates/game.html"};
		false ->
			wf:wire(#alert{text="Invalid Game"}),
			wf:redirect("/")
	end.

id() ->
	wf:path_info().

title() ->
	game:title(id()).


main_interaction() ->
	%% initialize the api
	wf:wire(#api{name=queue,tag=unused}),

	case player:name() of
		Undef when Undef==undefined;Undef=="" ->
			username_form();
		Name -> 
			canvas(Name)
	end.
		
username_form() ->
	wf:wire(username_cont,name,#validate{validators=[
		#is_required{}
	]}),
	#panel{id=username_form,class=username_form,body=[
		#label{text="What's your name?"},
		#textbox{id=name,text=""},
		#br{},
		#button{text="Continue",id=username_cont,postback=username}
	]}.

canvas(Playername) ->
	[
	#panel{id=headermessage,text=["Welcome ",Playername]},
	"<canvas class=game_canvas width=640 height=480>Get a browser that doesn't suck</canvas>",
	#panel{id=controls,body=controls()},
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
event(username) ->
	Name = wf:q(name),
	player:name(Name),
	wf:replace(username_form,canvas(Name));
event(erase) ->
	wf:wire("erase()");
event(_) -> 
	ok.

api_event(queue,_,ActionList) ->
	?PRINT(ActionList).
