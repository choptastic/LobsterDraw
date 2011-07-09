%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Spawnfest Pictionary".

game_list() ->
	[].

body() ->
	wf:wire(make_game,gamename,#validate{validators=[
		#is_required{}
	]}),	

	[
		#panel{class=header,text="Spawnfest Pictionary"},
		#singlerow{class=portal,cells=[
			#tablecell{body=[
				#panel{class=join,body=[
					#panel{class=joinheader,text="Join a game"},
					#panel{body=game_list()}
				]}
			]},
			#tablecell{body=[
				#panel{class=make,body=[
					#panel{class=makeheader,text="Make a game"},
					#panel{body=[
						#label{text="Game Name"},
						#textbox{text="",id=gamename},
						#br{},
						#button{postback=make_game,id=make_game,text="Create It!"}
					]}
				]}
			]}
		]}
	].
						
		
event(make_game) ->
	Name = wf:q(gamename),
	ID = game_master:new(Name),
	wf:redirect("/play/" ++ wf:to_list(ID)).

