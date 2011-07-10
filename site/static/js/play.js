var _color = "#000";
var _width = 3;
var _drawing = false;
var _lastcoords = null;
var _queue = [];
var _queue_timer = null;

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

function draw_queue()
{
	for(i=0;i<_queue.length;i++)
	{
		var action = _queue[i];
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
	_queue = [];
}


function send_queue()
{
	var to_send = _queue;
	_queue = [];

	for(i=0;i<to_send.length;i++)
	{
		to_send[i][0] = Bert.atom(to_send[i][0]);
	}
	if(to_send.length>0)
		page.queue(to_send);
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
	_queue.push(["line",_color,_width,x1,y1,x2,y2]);
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
	_queue.push(["fill",_color,x,y]);
}

function erase()
{
	var c = get_context();
	c.save();
	c.clearRect(0,0,c.canvas.width,c.canvas.height);
	c.restore();
}

function queue_erase()
{
	erase();
	_queue.push(["erase"]);
}

function get_coords(e)
{
	var offset = $(".game_canvas").offset();
	var x = e.pageX - offset.left;
	var y = e.pageY - offset.top;
	return {x:x,y:y}
}

function enable_drawing() 
{
	var canvas = $(".game_canvas")
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
	_queue_timer = setInterval(send_queue,300);
}

function disable_drawing()
{
	var canvas = $(".game_canvas")
		.unbind("mousedown")
		.unbind("mouseup")
		.unbind("mousemove");
	clearInterval(_queue_timer);
}

function timer_update(secs)
{
	_secondsleft = secs;
}

function round_over()
{
	disable_drawing();
}

