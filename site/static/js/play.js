var _color = "#000";
var _width = 3;
var _drawing = false;
var _lastcoords = null;
var _queue = [];

function get_context()
{
	var canvas = $(".game_canvas");
	if(canvas.getContext) 
		return canvas.getContext("2d");
	else{
		alert("Your browser doesn't support the canvas tag. Get one that doesn't suck.");
		location.href="http://chrome.google.com";
	}
}

function line(x1,y1,x2,y2)
{
	var c = get_context();
	c.moveTo(x1,y1);
	c.strokeStyle = _color;
	c.lineWidth = _width;
	c.lineTo(x2,y2);
	c.stroke();
	c.closePath();
}

function fill(x,y)
{
	var c = get_context();
	c.moveTo(x,y);
	c.fillStyle = _color;
	c.fill();
	c.closePath();
}

function erase()
{
	var c = get_context();
	var c.clearRect(0,0,context.canvas.width,context.canvas.height);
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
				line(	_lastcoords.x,
					_lastcoords.y,
					coords.x,
					coords.y);
				_lastcoords = coords;
			}
		});		
}
