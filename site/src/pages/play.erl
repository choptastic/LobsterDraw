-module(play).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

main() ->
	case game_server:exists(id()) of
		true ->
			#template{file="./site/templates/game.html"};
		false ->
			wf:wire(#alert{text="Invalid Game"}),
			wf:redirect("/")
	end.

id() ->	
	case wf:state(gameid) of
		undefined ->
			case string:to_integer(wf:path_info()) of
				{error,_} -> 0;
				{Int,_} -> wf:state(gameid,Int)
			end;
		ID -> ID
	end.

title() ->
	game_server:title(id()).


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
	Players = game_server:playerlist(id()),
	#table{rows=[
		lists:map(fun({Name,Score}) ->
			#tablerow{cells=[
				#tablecell{text=Name},
				#tablecell{text=Score}
			]}
		end,Players)
	]}.

clock() ->
	[].


controls() ->
	[].


%% -----------------actions and postbacksx-----------------------%%

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



%% ---------------comet game stuff--------------------%%

game_join() ->
	Pid = game_master:get_pid(id()),
	game_loop(GamePid).

game_loop(GamePid) ->
	process_flag(trap_exit,true),

	receive 
		{'EXIT',_,Message} ->
			game_server:leave(GamePid),
			exit(done)
		{correct,Player} ->
			ok;
		{join,Player} ->
			ok;
		{leave,Player} ->
			ok;
		{draw_queue,ActionList} ->
			ok;
		{new_round,Player} ->
			ok;
		{you_are_up} ->
			ok;
	end,
	game_loop(GamePid).
