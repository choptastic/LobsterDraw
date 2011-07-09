-module(play).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

id() ->
	wf:path_info().

title() ->
	[].

canvas() ->
	[].


playerlist() ->
	[].


clock() ->
	[].


controls() ->
	[].


