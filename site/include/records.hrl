-record(game,{
	id,
	pid,
	players=[],
	words=[],
	name="",
	lines=[],
	drawing_pid=undefined,
	going=false
}).

-record(player,{
	pid,
	name="guest",
	score=0,
	ready=false,
	correct=false
}).
