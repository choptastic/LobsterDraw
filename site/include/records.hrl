-record(game,{
	id,
	pid,
	word,
	players=[],
	words=[],
	name="",
	lines=[],
	drawing_pid=undefined,
	drawing_pids=[],
	timer_refs=[],
	verify_tref=undefined,
	going=false
}).

-record(player,{
	pid,
	name="guest",
	score=0,
	ready=false,
	correct=false,
	i_am_here=undefined
}).
