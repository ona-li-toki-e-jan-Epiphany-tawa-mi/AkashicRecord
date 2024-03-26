const canvas = document.getElementById("canvi");
const ctx = canvas.getContext("2d");
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;
let width = canvas.width;
let height = canvas.height;

let message = "Press a key.";

ctx.fillStyle = "#000000";
ctx.fillRect(0, 0, width, height);

ctx.font = "50px Comic Sans MS";
ctx.fillStyle = "#FFFFFF";
ctx.fillText("Press a", width / 2, height / 2);
ctx.fillText("  key.", width / 2, height / 2 + 50);

let drawRunner = setInterval(draw, 50);
/**
 * Draws stuff to the canvas.
 */
function draw() {
   canvas.width = window.innerWidth;
   canvas.height = window.innerHeight;
   width = canvas.width;
   height = canvas.height;
   
   ctx.fillStyle = "#000000";
   ctx.fillRect(0, 0, width, height);
   
   ctx.font = "50px Comic Sans MS";
   ctx.fillStyle = "#FFFFFF";
   ctx.fillText(message, width / 2 - ctx.measureText(message).width / 2, height / 2);
}

window.addEventListener("keydown", function(e) {
   let keyCode = e.keyCode;
   let key = e.key;
   
   message = "code: " + keyCode + " | key: " + key;
});