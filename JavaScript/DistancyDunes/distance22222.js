const canvas = document.getElementById("canvi");
const ctx = canvas.getContext("2d");
const width = canvas.width;
const height = canvas.height;

let squares = [[40,40,40,40], [width - 180,height - 90,20,20]];
let circles = [[300,10,50], [width - 200,height - 300,20]];

let leftBeef = false;
let roight = false;
let up = false;
let down = false;
let rollLeff = false;
let rollRoight = false;
let playerCoords = [width / 2, height / 2, 0, 1];
let playerSize = 10;
let seeAng = 0;
let seeVect = [];
let nearest = [0,"w",0];
let previous = [0,0];
let blinkCntr = 0;

const runProgram = setInterval(periodic, 20); 
let lastTime = new Date();
function periodic() { 
   let currTime = new Date();
   let deltaTime = (currTime - lastTime) / 1000;
   ctx.beginPath();
   ctx.fillStyle = "#000000";
   ctx.fillRect(0, 0, width, height);
   run(deltaTime);
   lastTime = new Date();
}

function run(dt) {
   if (up && !down) 
      playerCoords[1] -= (height / 10) * dt;
   else if (down && !up)
      playerCoords[1] += (height / 10) * dt;
   if (rollLeff && !rollRoight) 
      playerCoords[0] -= (width / 10) * dt;
   else if (rollRoight && !rollLeff)
      playerCoords[0] += (width / 10) * dt;
   if (leftBeef && !roight) {
      seeAng += 5 * dt;
      seeAng %= 360;
   } else if (roight && !leftBeef) {
      seeAng -= 5 * dt;
      seeAng %= 360;
   }
   if (seeAng < 0 )
      seeAng = 360 + seeAng;
   seeVect = [Math.sin(seeAng), Math.cos(seeAng)];
   
   if (playerCoords[0] - playerSize < 0)
      playerCoords[0] = playerSize;
   else if (playerCoords[0] + playerSize > width)
      playerCoords[0] = width - playerSize;
   if (playerCoords[1] - playerSize < 0)
      playerCoords[1] = playerSize;
   else if (playerCoords[1] + playerSize > height)
      playerCoords[1] = height - playerSize;
   
   nearest[0] = 1000000000;
   for (let x = 0; x < squares.length; x++) {
      let tenp = distanceFromSquareEdge(squares[x][0], squares[x][1], squares[x][2], squares[x][3], playerCoords[0], playerCoords[1], playerSize);
     if (tenp == NaN) {
        nearest[0] = 0;
        nearest[1] = "square";
        nearest[2] = x;
        break;
     } else if (tenp < nearest[0]) {
        nearest[0] = tenp;
        nearest[1] = "square";
        nearest[2] = x;
     }
   }
   for (let x = 0; x < circles.length; x++) {
      let tenp = distanceFromCircleEdge(circles[x][0], circles[x][1], circles[x][2], circles[x][2], playerCoords[0], playerCoords[1], playerSize);
     if (tenp == NaN) {
        nearest[0] = 0;
        nearest[1] = "circle";
        nearest[2] = x;
        break;
     } else if (tenp < nearest[0]) {
        nearest[0] = tenp;
        nearest[1] = "circle";
        nearest[2] = x;
     }
   }
   if (nearest[0] > 0) {
      if (nearest[0] <= 255)
         ctx.strokeStyle = "#ff" + Math.floor(nearest[0]).toString(16) + Math.floor(nearest[0]).toString(16);
      else if (nearest[0] < playerSize)
         ctx.strokeStyle = "#ff1010";
      else
         ctx.strokeStyle = "#ffffff";
      
      ctx.beginPath();
      ctx.arc(playerCoords[0], playerCoords[1], nearest[0], 0, Math.PI * 2);
      ctx.stroke();
      ctx.strokeStyle = "#ffffff";
   }
   if (nearest[0] < playerSize || nearest[0] == NaN) {
      playerCoords[0] = previous[0];
      playerCoords[1] = previous[1];
      
      if (blinkCntr >= 13)
         ctx.fillStyle = "#ff0000";
      else
         ctx.fillStyle = "#ffffff";
         
      blinkCntr++;
      blinkCntr %= 25;
   } else
      ctx.fillStyle = "#ffffff";
   
   previous[0] = playerCoords[0];
   previous[1] = playerCoords[1];
   
   ctx.beginPath();   
   ctx.arc(playerCoords[0], playerCoords[1], playerSize, 0, Math.PI * 2);
   ctx.fill();
   ctx.fillStyle = "#ffffff";
   
   for (let x = 0; x < squares.length; x++) {
      ctx.beginPath();
      ctx.rect(squares[x][0], squares[x][1], squares[x][2], squares[x][3]);
      ctx.fillStyle = "#ffffff";
      ctx.fill();
   }
   for (let x = 0; x < circles.length; x++) {
      ctx.beginPath();
      ctx.arc(circles[x][0], circles[x][1], circles[x][2], 0, Math.PI * 2);
      ctx.fillStyle = "#ffffff";
      ctx.fill();
   }
   
   if (nearest[1] == "square") {
      ctx.beginPath();
      ctx.rect(squares[nearest[2]][0], squares[nearest[2]][1], squares[nearest[2]][2], squares[nearest[2]][3]);
      ctx.fillStyle = "#1ff1ff";
      ctx.fill();
      ctx.fillStyle = "#ffffff";
   } else if (nearest[1] == "circle") {
      ctx.beginPath();
      ctx.arc(circles[nearest[2]][0], circles[nearest[2]][1], circles[nearest[2]][2], 0, Math.PI * 2);
      ctx.fillStyle = "#1ff1ff";
      ctx.fill();
   }
}

function distanceFromSquareEdge(x, y, dx, dy, x2, y2, r) {
   let center = [x + (dx / 2), y + (dy / 2)];
   let dist = Math.sqrt(Math.pow(center[0] - x2, 2) + Math.pow(center[1] - y2, 2));
   let ang = Math.asin(r / dist);
   let length = Math.min((dx / 2) / Math.cos(ang), (dy / 2) / Math.sin(ang));
   let secAng = Math.asin(Math.abs(y2 - center[1]) / dist) * 180 / Math.PI;
   if (center[0] > x2 && center[1] > y2) 
      secAng = (90 - secAng) + 90;
   else if (center[0] < x2 && center[1] < y2)
      secAng = (90 - secAng) + 270;
   else if (center[0] > x2 && center[1] < y2)
      secAng += 180;
   
   if (secAng < 90 && secAng > 0) {
      dist = Math.sqrt(Math.pow((center[0] + (0.3 * dx)) - x2, 2) + Math.pow((center[1] - (0.3 * dy)) - y2, 2));
      ang = Math.asin(r / dist);
      length = Math.min((dx / 4) / Math.cos(ang), (dy / 4) / Math.sin(ang));
   } else if (secAng < 170 && secAng > 85) {
      dist = Math.sqrt(Math.pow((center[0] - (0.3 * dx)) - x2, 2) + Math.pow((center[1] - (0.3 * dy)) - y2, 2));
      ang = Math.asin(r / dist);
      length = Math.min((dx / 4) / Math.cos(ang), (dy / 4) / Math.sin(ang));
   } else if (secAng < 270 && secAng > 180) {
      dist = Math.sqrt(Math.pow((center[0] - (0.3 * dx)) - x2, 2) + Math.pow((center[1] + (0.3 * dy)) - y2, 2));
      ang = Math.asin(r / dist);
      length = Math.min((dx / 4) / Math.cos(ang), (dy / 4) / Math.sin(ang));
   } else if (secAng < 350 && secAng > 280) {
      dist = Math.sqrt(Math.pow((center[0] + (0.3 * dx)) - x2, 2) + Math.pow((center[1] + (0.3 * dy)) - y2, 2));
      ang = Math.asin(r / dist);
      length = Math.min((dx / 4) / Math.cos(ang), (dy / 4) / Math.sin(ang));
   }
   
   return dist - length;
}

function distanceFromCircleEdge(x, y, r11, r12, x2, y2, r) {
   let center = [x, y];
   let dist = Math.sqrt(Math.pow(center[0] - x2, 2) + Math.pow(center[1] - y2, 2));
   let ang = Math.asin(r / dist);
   let length = Math.min(r11 / Math.cos(ang), r12 / Math.sin(ang));
  
   return dist - length;
}

window.addEventListener("keydown", function(e){
   let key = e.keyCode;
   if (key == 37)
      leftBeef = true;
   if (key == 39)
      roight = true;
   if (key == 87)
      up = true;
   if (key == 83)
      down = true;
   if (key == 65)
      rollLeff = true;
   if (key == 68)
      rollRoight = true;
});
window.addEventListener("keyup", function(e){
   let key = e.keyCode;
   if (key == 37)
      leftBeef = false;
   if (key == 39)
      roight = false;
   if (key == 87)
      up = false;
   if (key == 83)
      down = false;
   if (key == 65)
      rollLeff = false;
   if (key == 68)
      rollRoight = false;
});