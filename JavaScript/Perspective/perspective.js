var canvas = document.getElementById("myCanvas");
var ctx = canvas.getContext("2d");
var x = 1; 
var y = 0;
var dx = 0;
var dy = 0;
var cntr = 0; 
var stop = false;

var drawT = setInterval(doThing, 1);

function doThing() {
  ctx.beginPath();
  
  dx = (abs(Math.sin(cntr * 1.8)) * 40) + 55;
  dy = (abs(Math.sin(cntr) * 1.8) * 16) + 36.6;
  
  if (x == 0 && y == 0) {
    clearInterval(drawT); 
  }
  
  if (x < 600 && y == 0) {
    ctx.moveTo(x, y + dy);
    x++;
  } else if (y < 400 && x == 600) {
    ctx.moveTo(x - dx, y);
    y++;
  } else if (y == 400 && x > 0) {
    ctx.moveTo(x, y - dy);
    x--;
  } else if (x == 0 && y > 0) {
    ctx.moveTo(x + dx, y);
    y--;
  }
  
  ctx.lineTo(300, 200);
  if (x != 0 || y != 0) {
    ctx.stroke(); 
  }
  
  cntr++;
}

function abs(a) {
  if (a < 0) {
    a *= -1;
  }
  return a;
}
