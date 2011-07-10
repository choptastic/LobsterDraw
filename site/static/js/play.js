var _color = "#000";
var _width = 3;
var _drawing = false;
var _lastcoords = null;
var _out_queue = [];
var _queue_timer = null;
var _game_timer = null;
var _secondsleft = 0;

function get_context()
{
	var canvas = $(".game_canvas").get(0);
	if(canvas.getContext) 
		return canvas.getContext("2d");
	else{
		alert("Your browser doesn't support the canvas tag. Get one that doesn't suck.");
		//location.href="http://chrome.google.com";
	}
}

function load_queue(_in_queue)
{
	for(i=0;i<_in_queue.length;i++)
	{
		var action = _in_queue[i];
		switch(action[0])
		{
			case "line":
				line(action[1],action[2],action[3],action[4],action[5],action[6]);
				break;
			case "fill":
				fill(action[1],action[2],action[3]);
				break;
			case "erase":
				erase();
				break;
		}
	}
	_in_queue = [];
}



function send_queue()
{
	var to_send = _out_queue;
	_out_queue = [];

	for(i=0;i<to_send.length;i++)
	{
		to_send[i][0] = Bert.atom(to_send[i][0]);
	}
	if(to_send.length>0)
		page.pict_api(to_send);
}


function setcolor(color)
{
	_color = color;	
}

function setwidth(width)
{
	_width = width;
}

function line(color,width,x1,y1,x2,y2)
{
	var c = get_context();
	c.beginPath();
	c.moveTo(x1,y1);
	c.strokeStyle = color;
	c.lineWidth = width;
	c.lineTo(x2,y2);
	c.stroke();
	c.closePath();
}

function queue_line(x1,y1,x2,y2)
{
	line(_color,_width,x1,y1,x2,y2);
	_out_queue.push(["line",_color,_width,x1,y1,x2,y2]);
}

function fill(color,x,y)
{
	var c = get_context();
	c.moveTo(x,y);
	c.fillStyle = color;
	c.fill();
	c.closePath();
}

function queue_fill(x,y)
{
	fill(_color,x,y);
	_out_queue.push(["fill",_color,x,y]);
}

function erase()
{
	var c = get_context();
	c.save();
	c.clearRect(0,0,c.canvas.width,c.canvas.height);
	c.restore();
}

//We're clearing the input a few seconds after submission so that we don't submit empty text
function clear_input()
{
	setTimeout(function(){
		$("input[type=text].guess").va("").focus();
	},50);
}

function queue_erase()
{
	erase();
	_out_queue.push(["erase"]);
}

function get_coords(e)
{
	var offset = $(".game_canvas").offset();
	var x = e.pageX - offset.left;
	var y = e.pageY - offset.top;
	return {
		x:parseInt(x),
		y:parseInt(y)
	}
}

function enable_drawing() 
{
	var canvas = $(".game_canvas")
		.addClass("game_up")
		.mousedown(function(e) {
			_drawing = true;
			_lastcoords = get_coords(e);
		})
		.mouseup(function(){
			_drawing=false;
		})
		.mousemove(function(e){
			if(_drawing) {
				coords = get_coords(e);
				queue_line(	
					_lastcoords.x,
					_lastcoords.y,
					coords.x,
					coords.y);
				_lastcoords = coords;

			}
		});
	_queue_timer = setInterval(send_queue,1000);
}

function disable_drawing()
{
	var canvas = $(".game_canvas")
		.removeClass("game_up")
		.unbind("mousedown")
		.unbind("mouseup")
		.unbind("mousemove");
	clearInterval(_queue_timer);
}

function timer_update(secs)
{
	$(".clock").text(secs);

	if(_game_timer)
		clearTimeout(_game_timer);
	var next_time = secs-1;
	if(next_time >=0)
		_game_timer = setTimeout(function(){timer_update(next_time)},1000);
}

function round_over()
{
	disable_drawing();
}

function start_round()
{
	erase();
	timer_update(60);
}

function i_am_here()
{
	page.i_am_here();
}

$("body").unload(function(){
	page.leave();
});
