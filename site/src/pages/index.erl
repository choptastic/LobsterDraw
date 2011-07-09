%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Spawnfest Pictionary".

game_list() ->
	[].

body() ->
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
						#textbox{text=""},
						#br{},
						#button{postback=make_game,text="Create It!"}
					]}
				]}
			]}
		]}
	].
						
		
