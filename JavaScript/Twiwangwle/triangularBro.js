let canvas = document.getElementById("myCanvas");
let ctx = canvas.getContext("2d");
let width = canvas.width; 
let height = canvas.height;

let maxpoints = 100000;
const cps = 20;
const rotate = true;

const a = [width/2, height/13];
const b = [width/9, height/1.1];
const c = [width/1.1, height/1.1];
let str = [width/3, height/1.5];
let mve = [0, 0]; 
let ran;
let drawBuff = [];
const timeInterval = 1000 / 20;
let currAngle = 0;

ctx.fillRect(0,0,width,height);
ctx.fillStyle = "rgba(255,255,255)";


drawBuff.push([a[0], a[1]])
drawBuff.push([b[0], b[1]])
drawBuff.push([c[0], c[1]])
drawBuff.push([mve[0], mve[1]])

setInterval(drawDude, timeInterval);

function drawDude() {
   if (drawBuff.length < maxpoints) {
      ran = Math.floor(Math.random() * 3) + 1;
   
      if (ran == 1) {
         mve[0] = (str[0] + a[0]) / 2;
         mve[1] = (str[1] + a[1]) / 2;
      } else if (ran == 2) {
         mve[0] = (str[0] + b[0]) / 2;
         mve[1] = (str[1] + b[1]) / 2;
      } else {
         mve[0] = (str[0] + c[0]) / 2;
         mve[1] = (str[1] + c[1]) / 2;
      }
   
      drawBuff.push([mve[0], mve[1]])
   
      str[0] = mve[0];
      str[1] = mve[1];
   }
   
   ctx.fillStyle = "rgba(0,0,0)";
   ctx.fillRect(0,0,width,height);
   
   ctx.fillStyle = "rgba(255,255,255)";
   for (let x = 0; x < drawBuff.length; x++) {
      ctx.beginPath();
      
      if (rotate) {
         let translatedX = drawBuff[x][0] - width / 2;
         let translatedY = drawBuff[x][1] - height / 2;
         let rotatedX = translatedX * Math.cos(currAngle) - translatedY * Math.sin(currAngle);
         let rotatedY = translatedX * Math.sin(currAngle) + translatedY * Math.cos(currAngle);
         
         ctx.arc(rotatedX + width / 2, rotatedY + height / 2, 1, 0, 2 * Math.PI);
         
         currAngle += 0.000001 * timeInterval;
         currAngle %= 360;
      } else
         ctx.arc(drawBuff[x][0], drawBuff[x][1], 1, 0, 2 * Math.PI);
      ctx.fill(); 
   }
}