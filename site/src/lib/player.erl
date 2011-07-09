-module(player).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-compile(export_all).

name(Name) ->
	wf:user(Name),
	wf:cookie(name,Name,"/",50000).

name() ->
	wf:coalesce([wf:user(),wf:cookie(name)]).
