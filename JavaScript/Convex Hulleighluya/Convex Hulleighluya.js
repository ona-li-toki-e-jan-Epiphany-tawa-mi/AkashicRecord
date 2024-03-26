/*
   A convex hull detection algorithim using a rotating pole.
   When the lighter part of the pole touches a point it moves to that point, and saves it to a list.
   This list is then used to draw a path around the shape.

   The pole always starts in the middle of the screen, slightly above the heighest point.
   Left click to reset.
   
   It gets weird if you move to other tabs and leave it running, why not give it a shot?
*/

const canvas = document.getElementById("canvi");
const ctx = canvas.getContext("2d");
const width = canvas.width;
const height = canvas.height;

const backgroundColor = "#000000";
const pointColor = "#ffffff";
const selectedPointColor = "#6699ff";
const barColor1 = "#afafaf";
const barColor2 = "#505050";
const hullColor = "#ff3f3f";
const numPoints = 25;
const barSpeed = 0.5;
const timeSteps = 4;

let points = [];
const pointSize = 10;
let bar = {x: width / 2, y: 0, width: pointSize / 2, length: width + height, angle: 0, currPt: null};
let traveldPts = [];
let travelDone = false;

calcPoints();

const runProgram = setInterval(periodic, 1); 
const drawOutput = setInterval(draw, 20); 
let lastTime = new Date();
function periodic() { 
      for (let i = 0; i < timeSteps; i++) {
      let currTime = new Date();
      let deltaTime = (currTime - lastTime) / 1000;
      run(deltaTime);
      lastTime = new Date();
   }
}

function calcPoints() {
   points = []
   for (let i = 0; i < numPoints; i++) 
      points.push({x: Math.random() * width * 0.75 + width * 0.125,
                  y: Math.random() * height * 0.75 + height * 0.125});
   
   let highestPoint = 0;
   for (let i = 0; i < points.length; i++) {
      currY = points[i].y;
      
      if (currY > highestPoint)
         highestPoint = currY + (pointSize + bar.width) * 2;
   }
   bar.y = highestPoint;
   bar.x = width / 2
   bar.angle = 0;
   bar.currPt = null;
   
   traveldPts = [];
   travelDone = false;
}

function contains(list, element) {
   let inList = false;
   
   for (let k = 0; k < list.length; k++)
      if (list[k] == element) {
         inList = true;
      }
   
   return inList;
}

function run(dt) {
   bar.angle -= barSpeed * dt;
   let barEndPtCord = {x1: Math.cos(bar.angle) * bar.length + bar.x, 
                       y1: Math.sin(bar.angle) * bar.length + bar.y,
                       x2: Math.cos(bar.angle) * -bar.length + bar.x, 
                       y2: Math.sin(bar.angle) * -bar.length + bar.y};
   
   for (let i = 0; i < points.length; i++) {
      let dist2pt = Math.sqrt(Math.pow(bar.x - points[i].x, 2) + Math.pow(bar.y - points[i].y, 2));
      let adjEndPt = {x: (barEndPtCord.x1 - bar.x) / bar.length * dist2pt + bar.x, 
                      y: (barEndPtCord.y1 - bar.y) / bar.length * dist2pt + bar.y};
      let ptEndPtDst = Math.sqrt(Math.pow(adjEndPt.x - points[i].x, 2) + Math.pow(adjEndPt.y - points[i].y, 2));
      
      if (ptEndPtDst <= pointSize + bar.width / 2 && bar.currPt != i) {
         bar.x = points[i].x;
         bar.y = points[i].y;
         bar.currPt = i;
         
         if (traveldPts.length > 2 && traveldPts[0] == i)
            travelDone = true;
         if (!travelDone && !contains(traveldPts, i))
            traveldPts.push(i);
      }
   }
}

function draw() {
   ctx.beginPath();
   ctx.fillStyle = backgroundColor;
   ctx.fillRect(0, 0, width, height);
   
   if (traveldPts.length >= 2) {
      ctx.beginPath()
      ctx.strokeStyle = hullColor;
      ctx.lineWidth = pointSize / 2;
      
      ctx.moveTo(points[traveldPts[0]].x, height - points[traveldPts[0]].y)
      for (let i = 1; i < traveldPts.length; i++) {
         ctx.lineTo(points[traveldPts[i]].x, height - points[traveldPts[i]].y)
      }
      ctx.lineTo(points[traveldPts[0]].x, height - points[traveldPts[0]].y)
      ctx.stroke();
   }
   
   let barEndPtCord = {x1: Math.cos(bar.angle) * bar.length + bar.x, 
                       y1: Math.sin(bar.angle) * bar.length + bar.y,
                       x2: Math.cos(bar.angle) * -bar.length + bar.x, 
                       y2: Math.sin(bar.angle) * -bar.length + bar.y};
   
   ctx.beginPath();
   ctx.lineWidth = bar.width;
   ctx.strokeStyle = barColor1;
   ctx.moveTo(barEndPtCord.x1, height - barEndPtCord.y1);
   ctx.lineTo(bar.x, height - bar.y);
   ctx.stroke();
   
   ctx.beginPath();
   ctx.strokeStyle = barColor2;
   ctx.moveTo(barEndPtCord.x2, height - barEndPtCord.y2);
   ctx.lineTo(bar.x, height - bar.y);
   ctx.stroke();
   
   for (let i = 0; i < points.length; i++) {
      ctx.beginPath();
      ctx.arc(points[i].x, height - points[i].y, pointSize, 0, 2 * Math.PI);
      ctx.fillStyle = pointColor;
      ctx.fill();
   }
   
   if (bar.currPt != null) {
      ctx.beginPath();
      ctx.arc(points[bar.currPt].x, height - points[bar.currPt].y, pointSize, 0, 2 * Math.PI);
      ctx.fillStyle = selectedPointColor;
      ctx.fill();
   }
}

window.addEventListener("click", function() {
   calcPoints();
});