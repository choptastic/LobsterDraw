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
	CometPid = wf:comet(fun() -> game_join(Playername) end),
	wf:state(cometpid,CometPid),

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

ready_button() ->
	#button{text="Ready",postback=ready}.

clock() ->
	#panel{id=clock,body=[
		ready_button()
	]}.


controls() ->
	[].


activitylog() ->
	#panel{id=activitylog}.


%% -----------------actions and postbacksx-----------------------%%

event(username) ->
	Name = wf:q(name),
	player:name(Name),
	wf:replace(username_form,canvas(Name));
event(ready) ->
	send_to_comet(fun(GamePid) -> game_server:ready(GamePid) end);
event(unready) ->
	send_to_comet(fun(GamePid) -> game_server:unready(GamePid) end);
event(guess) ->
	Guess = wf:q(guess),
	send_to_comet(fun(GamePid) -> game_server:guess(GamePid,Guess) end);
event(erase) ->
	wf:wire("erase()");
event(_) -> 
	ok.

api_event(queue,_,ActionList) ->
	?PRINT(ActionList).



%% ---------------comet game stuff--------------------%%

%% This will send a function to the comet process to be executed there, so that the pid is ready properly
%% The alternative would be to allow multiple pids per client, this way seems easier right now
%% Maybe it's a mistake
%% The fun must be arity 1 and the only argument should be GamePid, which will be passed in by the loop
send_to_comet(Fun) ->
	Pid = wf:state(cometpid),
	Pid ! {from_page,Fun}.

%% Join the game and initiate the comet loop
game_join(Name) ->
	GamePid = game_master:get_pid(id()),
	game_server:join(GamePid,Name),
	game_loop(GamePid).

game_loop(GamePid) ->
	process_flag(trap_exit,true),

	receive 
		{'EXIT',_,Message} ->
			game_server:leave(GamePid),
			exit(done);
		{join,Player} ->
			in_join(Player);
		{leave,Player} ->
			in_leave(Player);
		{correct,Player} ->
			in_correct(Player);
		{ready,Player} ->
			in_ready(Player);
		{unready,Player} ->
			in_unready(Player);
		{all_correct} ->
			in_all_correct();
		{queue,ActionList} ->
			in_queue(ActionList);
		{new_round,Player} ->
			in_new_round(Player);
		{you_are_up,Word} ->
			in_you_are_up(Word);
		{timer_update,SecondsLeft} ->
			in_timer_update(SecondsLeft);
		{from_page,Fun} ->
			Fun(GamePid)
	end,
	wf:flush(),
	game_loop(GamePid).

in_join(Player) ->
	wf:update(


in_you_are_up(Word) ->
	wf:update(headermessage,"It's your turn to draw. Your word: " ++ Word).

in_new_round(Player) ->
	wf:update(headermessage,"It's " ++ Player ++ "'s turn to draw").
