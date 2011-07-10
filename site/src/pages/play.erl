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
				{Int,_} -> 
					wf:state(gameid,Int),
					Int
			end;
		ID -> ID
	end.

title() ->
	game_server:title(id()).


main_interaction() ->
	%% initialize the api
	wf:wire(#api{name=pict_api,tag=unused}),
	wf:wire(#api{name=leave,tag=unused}),
	wf:wire(#api{name=i_am_here,tag=unused}),

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
	{ok,CometPid} = wf:comet(fun() -> game_join(Playername) end),
	wf:state(cometpid,CometPid),

	case game_server:is_going(id()) of
		true -> catch_me_up();
		false -> wf:wire("enable_drawing()")
	end,

	[
		#panel{id=headermessage,class=headermessage,text=["Welcome ",Playername]},
		"<canvas class=game_canvas width=500 height=300>Get a browser that doesn't suck</canvas>",
		#panel{id=controls,class=chatcontrols,body=canvascontrols()}
	].
	

playerlist() ->
	Players = game_server:playerlist(id()),
	#table{id=playerlist,rows=[
		lists:map(fun(P=#player{}) ->

			Ready = case P#player.ready of
				true -> " (ready)";
				false -> ""
			end,

			#tablerow{cells=[
				#tablecell{class=readystatus,text=Ready},
				#tablecell{class=playername,text=P#player.name},
				#tablecell{class=score,text=P#player.score}
			]};
			(_) -> []	%% For outdated records
		end,Players)
	]}.

ready_button() ->
	#button{class=ready,text="Ready",postback=ready}.

unready_button() ->
	#button{class=unready,text="Not Ready",postback=unready}.

clock() ->
	#panel{id=clock,body=[
		ready_button()
	]}.

color_button(Color) ->
	Pixel = #image{image="/images/px.gif",style="background:" ++ Color},
	#link{class=color_button,body=Pixel,actions=[
		#event{type=click,actions=[
			#script{script="setcolor(\"" ++ Color ++ "\")"}
		]}
	]}.

color_row(Row) ->
	#panel{class=color_row,body=[color_button(Color) || Color <- Row]}.


%% Make a color picker based on the cartesian product of 0,8,f, amounting to enough color combos, ie #000, #8f0, #ff8, etc
colorpicker() ->
	Ranges = sofs:set("08f"),
	Prod = sofs:product({Ranges,Ranges,Ranges}),
	Tuples = sofs:to_external(Prod),
	Colors = ["#" ++ tuple_to_list(T) || T<-Tuples],
	Top = lists:sublist(Colors,1,9),
	Mid = lists:sublist(Colors,10,9),
	Bot = lists:sublist(Colors,19,9),

	[color_row(Top),color_row(Mid),color_row(Bot)].

erase_button() ->
	#button{text="Erase Canvas",actions=[
		#event{type=click,actions=[
			#script{script="queue_erase()"}
		]}
	]}.



canvascontrols() ->
	#singlerow{class=canvascontrols,cells=[
		#tablecell{body=colorpicker()},
		#tablecell{class=erase_button,body=erase_button()}
	]}.


chatcontrols() ->
	#panel{class=[guess,game_up],body=[
		#textbox{class=[guess,game_up],id=guess,text="",postback=guess}
	]}.


you_got_it(Word) ->
	#panel{class=you_got_it,text="You got it! The word was " ++ Word}.


activitylog() ->
	#panel{id=activitylog}.


update_playerlist() ->
	wf:replace(playerlist,playerlist()).

%% -----------------actions and postbacksx-----------------------%%

event(username) ->
	Name = wf:q(name),
	player:name(Name),
	wf:replace(username_form,canvas(Name));
event(ready) ->
	send_to_comet(fun(GamePid) -> game_server:ready(GamePid) end),
	wf:update(clock,unready_button());
event(unready) ->
	send_to_comet(fun(GamePid) -> game_server:unready(GamePid) end),
	wf:update(clock,ready_button());
event(guess) ->
	Guess = wf:q(guess),
	send_to_comet(fun(GamePid) -> game_server:guess(GamePid,Guess) end),
	wf:wire("$(obj('guess')).val('').focus();");
event(erase) ->
	wf:wire("erase()");
event(_) -> 
	ok.


api_event(i_am_here,_,[]) ->
	send_to_comet(fun(GamePid) -> game_server:i_am_here(GamePid) end);

api_event(leave,_,[]) ->
	send_to_comet(fun(GamePid) -> game_server:leave(GamePid) end);

api_event(pict_api,_,[ActionList]) ->
	send_to_comet(fun(GamePid) -> game_server:queue(GamePid,ActionList) end).




%% ---------------comet game stuff--------------------%%

%% This will send a function to the comet process to be executed there, so that the pid is ready properly
%% The alternative would be to allow multiple pids per client, this way seems easier right now
%% Maybe it's a mistake
%% The fun must be arity 1 and the only argument should be GamePid, which will be passed in by the loop
send_to_comet(Fun) when is_function(Fun,1) ->
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
		{'EXIT',_,_Message} ->
			game_server:leave(GamePid),
			exit(done);
		{join,Player} ->
			in_join(Player);
		{leave,Player} ->
			in_leave(Player);
		{correct,Player,Points} ->
			in_correct(Player,Points);
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
		{round_over,Word} ->
			in_round_over(Word);
		{game_starting} ->
			in_game_starting();
		{you_are_up,Word} ->
			in_you_are_up(Word);
		{you_got_it,Word} ->
			in_you_got_it(Word);
		{timer_update,SecondsLeft} ->
			in_timer_update(SecondsLeft);
		{get_ready,Player} ->
			in_get_ready(Player);
		{game_over} ->
			in_game_over();
		{are_you_there} ->
			in_are_you_there();

		{from_page,Fun} ->
			Fun(GamePid)
	after 5000 ->
			continue
	end,
	wf:flush(),
	game_loop(GamePid).

add_message(Class,Msg) ->
	wf:insert_bottom(activitylog,#panel{text=Msg,class=Class}).
	
in_join(Player) ->
	add_message(log_join,Player ++ " has joined the game"),
	update_playerlist().

in_leave(Player) ->
	add_message(log_leave,Player ++ " has left the game "),
	update_playerlist().

in_correct(Player,Points) ->
	add_message(log_correct,Player ++ " got it for " ++ wf:to_list(Points) ++ " points"),
	update_playerlist().

in_you_got_it(Word) ->
	wf:update(controls,you_got_it(Word)).

in_ready(Player) ->
	add_message(log_ready,Player ++ " is ready to start"),
	update_playerlist().

in_unready(Player) ->
	add_message(log_unready,Player ++ " is not ready to start"),
	update_playerlist().

in_all_correct() ->
	add_message(log_correct,"Everyone got it!").

in_queue(Queue) ->
	NewQueue = encode_queue(Queue),
	wf:wire("load_queue(" ++ NewQueue ++ ");").

in_game_starting() ->
	add_message(log_starting,"The Game is about to start.  ").

in_get_ready(Player) ->
	wf:update(headermessage,"It will be " ++ Player ++ "'s turn to draw!"),
	wf:update(clock,"&#8734;").

in_you_are_up(Word) ->
	wf:update(headermessage,"It's your turn to draw. Your word: " ++ Word),
	wf:update(controls,canvascontrols()),
	wf:wire("start_round();"),
	wf:wire("enable_drawing();").

in_new_round(Player) ->
	wf:update(headermessage,"It's " ++ Player ++ "'s turn to draw"),
	wf:update(controls,chatcontrols()),
	wf:wire("disable_drawing();"),
	wf:wire("start_round();").

in_round_over(Word) ->
	wf:wire("round_over()"),
	add_message(log_round_over,"Round Over. The word was " ++ Word).

in_timer_update(SecondsLeft) ->
	wf:wire("timer_update(" ++ wf:to_list(SecondsLeft) ++ ");").


in_game_over() ->
	wf:update(headermessage,"Game Over"),
	wf:wire("game_over()"),
	wf:update(clock,ready_button()).

in_are_you_there() ->
	%% We call i_am_here() from are_you_there()
	%% If the javascript is running, it'll respond accordingly
	wf:wire("page.i_am_here();").

catch_me_up() ->
	[].

%% I use a bunch of ++'s here. I know it's slow, so sue me
encode_queue(ActionList) ->
	Q1 = lists:map(fun(Action) ->
		EncodedArgs = lists:map(fun encode_queue_item/1,Action),
		JSAction = string:join(EncodedArgs,","),
		"[" ++ JSAction ++ "]"
	end,ActionList),
	"[" ++ string:join(Q1,",") ++ "]".


encode_queue_item(N) when is_integer(N) ->
	wf:to_list(N);
encode_queue_item(AS) when is_atom(AS);is_list(AS) ->
	"\"" ++ wf:to_list(AS) ++ "\"";
encode_queue_item(F) when is_float(F) ->
	encode_queue_item(round(F));
encode_queue_item(_Other) ->
	"null".
