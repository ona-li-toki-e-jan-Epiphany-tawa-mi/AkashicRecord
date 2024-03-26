const canvas = document.getElementById("myCanvas");
const ctx = canvas.getContext("2d");
const width = canvas.width;
const height = canvas.height;
const basecolor = [0x0F, 0xFF, 0xFF];
const size = 10;
ctx.fillStyle = "rgba(" + basecolor[0] + "," + basecolor[1] + "," + basecolor[2]

const runProgram = setInterval(periodic, 20); 
var lastTime = new Date();

var reverseGravity = false;
var coords = [width / 3, height / 2, 0];
const yMomentumMax = 2;

var trail = [];
var trailPolarity;

function periodic() { 
   var currTime = new Date();
   var deltaTime = (currTime - lastTime) / 1000;
   run(deltaTime);
   lastTime = new Date();
}

function run(dt) {
   ctx.fillStyle = "rgba(" + basecolor[0] + "," + basecolor[1] + "," + basecolor[2];
   ctx.fillRect(0, 0, width, height);
   
   if (trail.length > 290)
      trail.shift();
   if (trail.length > 0)
      for (let x = 0; x < trail.length; x++) {
         trail[x][0] -= 100 * dt;
      }
   
   if (reverseGravity && coords[2] > -yMomentumMax)
      coords[2] -= (yMomentumMax * 1.5) * dt;
   else if (coords[2] < yMomentumMax)
      coords[2] += (yMomentumMax * 1.5) * dt;
   coords[1] += coords[2];
   
   if (coords[1] - size < 0) {
      coords[1] = size;
      coords[2] = yMomentumMax + 1;
   } else if (coords[1] + size > height) {
      coords[1] = height - size;
      coords[2] = -(yMomentumMax + 1);
   }
   
   if (trail.length > 0) 
      for (let x = trail.length - 1; x >= 0; x--) {
         
         if (trail[x][2][0] < basecolor[0])
            trail[x][2][0] += 60 * dt;
         else if (trail[x][2][0] > basecolor[0])
            trail[x][2][0] = basecolor[0];
         if (trail[x][2][1] < basecolor[1])
            trail[x][2][1] += 60 * dt;
         else if (trail[x][2][1] > basecolor[1])
            trail[x][2][1] = basecolor[1];
         if (trail[x][2][2] < basecolor[2])
            trail[x][2][2] += 60 * dt;   
         else if (trail[x][2][2] > basecolor[2])
            trail[x][2][2] = basecolor[2];
         
         ctx.fillStyle = "rgba(" + trail[x][2][0] + "," + trail[x][2][1] + "," + trail[x][2][2] + ")";
         ctx.beginPath();
         ctx.arc(trail[x][0], trail[x][1], size / 1.1, 0, 2 * Math.PI);
         ctx.fill();
      }
   ctx.fillStyle = "rgba(0,0,0)";
   ctx.beginPath();
   ctx.arc(coords[0], coords[1], size, 0, 2 * Math.PI);
   ctx.fill();
   
   if (Math.floor(Math.random() * 2) == 1)
      trailPolarity = -1;
   else
      trailPolarity = 1;
   
   trail.push([coords[0], coords[1] + (Math.random() * 3 * trailPolarity), [0x0, 0x0, 0x0]]);
}

window.addEventListener("keydown", function(){
   reverseGravity = true;
});
window.addEventListener("keyup", function(){
   reverseGravity = false;
});