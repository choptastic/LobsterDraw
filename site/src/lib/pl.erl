%% pl, a supplementary module for dealing with proplists in a way I find more convenient
%% Also, I know it goes against the "Erlang way" to put the collection first in the argument list
%% But that's the way that feels more natural to me so that's how I have this particular module written

-module(pl).

-export([set/2,set/3,
	get/2,get/3,
	delete/2]).

% Can set with set(Proplist,Key,Val) or Set(PropList,{Key,Val}) or Set(Proplist,[{Key,Val},{Key,Val}])
% retrieve values with get(Proplist,Key) or get(Proplist,Key,IfNotFound)
% delete keys with delete(Proplist,Key)


set(PL,Key,Val) ->
	set(PL,{Key,Val}).

set(PL,{Key,Val}) ->
	% delete a matching key if exists and prepend {K,V} to the list. No guarantee of proplist order
	[{Key,Val} | delete(PL,Key)];
set(PL,[]) ->
	PL;
set(PL,[{Key,Val} | Rest]) ->
	NewPL = set(PL,{Key,Val}),
	set(NewPL,Rest).


get(PL,Keys,Default) when is_list(Keys),is_list(PL) ->
	[get(PL,Key,Default) || Key<-Keys];
get(PL,Key,Default) when is_list(PL) ->
	proplists:get_value(Key,PL,Default).


get(PL,Keys) when is_list(Keys),is_list(PL) ->
	get(PL,Keys,"");
get(PL,Key) when is_list(PL) ->
	get(PL,Key,"").


delete(PL,[Key|RestKeys]) when is_list(PL) ->
	NewPL = delete(PL,Key),
	delete(NewPL,RestKeys);
delete(PL,Key) when is_list(PL) ->
	[{K,V} || {K,V} <- PL,K /= Key].

