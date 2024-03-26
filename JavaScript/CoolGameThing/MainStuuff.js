import * as CanvasHandler from '/renderingStuuf/CanvasHandler.js'; 
import * as Cuboid from '/coordinations/shapes/Cuboid.js';
import * as WireFrameRenderer from '/renderingStuuf/WireFrameRenderer.js'; 
import * as Camera from '/renderingStuuf/Camera3d.js'; 
import * as Vector3d from '/coordinations/Vector3d.js'; 
import * as Vector2d from '/coordinations/Vector2d.js'; 

let calculationsPerSecond = 20;
let framesPerSecond = 60;

let gameLoopRunner = window.setInterval(run, 1000 / calculationsPerSecond);
let renderLoopRunner = window.setInterval(render, 1000 / framesPerSecond);

// Figure out a name (possibly Fortilituda).

// Implement Bresenham's algorithim for drawing lines in CanvasHandler

// Add control code to correct error in the elapsed time every so often.

// Make a 2d variant of Camera3d.

// Find the matrix math that stops the cuboids from streching to the screen.

// Define some more shapes.
// Figure out quaternions.

// Fix the weird white border thing. (the window has decimal pixels, the canvas does not.)
//  Also find some way to do CanvasHandler#setColorMatrixSize() every so often.

// Make GitHub repository and sync with this project. The commutative property stands.

let cube = new Cuboid.Cuboid(0.5, 0.5, 0.5, 0, 0, 4, 0, 0, 0);
let cube2 = new Cuboid.Cuboid(1, 1, 1, 3, 5, 4, 12, 0, 0);
let camera = new Camera.Camera3d(0, 0, 0, 0, 0, 0, 70);

let forward = false;
let backward = false;
let left = false;
let right = false;
let up = false;
let down = false;

let rotateLeft = false;
let rotateRight = false;
let rotateUp = false;
let rotateDown = false;
let rotateWeirdLeft = false;
let rotateWeirdRight = false;

let lastTime = Date.now();
let elapsedTime = 0;
/**
 * The primary loop for the logic of this pseudo-game.
 */
function run() {
    let currentTime = Date.now();
    let deltaTime = (currentTime - lastTime) / 1000;
   
    elapsedTime += deltaTime;
    if (elapsedTime > 100000 || elapsedTime < 0)
        elapsedTime = 0;
    
    if (forward && !backward) {
        let deltaPosition = new Vector2d.Vector2d(0, 1).rotate(Math.abs(camera.rotation.y)).scaleUnsafely(deltaTime);
        
        camera.position.translateUnsafely(deltaPosition.x, 0, deltaPosition.y);
    
    } else if (!forward && backward) {
        let deltaPosition = new Vector2d.Vector2d(0, 1).rotate(Math.abs(camera.rotation.y)).scaleUnsafely(-deltaTime);
        
        camera.position.translateUnsafely(deltaPosition.x, 0, deltaPosition.y);
    }
    
    if (left && !right) {
        let deltaPosition = new Vector2d.Vector2d(0, 1).rotate(Math.abs(camera.rotation.y) + Math.PI / 2).scaleUnsafely(deltaTime);
        
        camera.position.translateUnsafely(deltaPosition.x, 0, deltaPosition.y);
    
    } else if (!left && right) {
        let deltaPosition = new Vector2d.Vector2d(0, 1).rotate(Math.abs(camera.rotation.y) - Math.PI / 2).scaleUnsafely(deltaTime);
        
        camera.position.translateUnsafely(deltaPosition.x, 0, deltaPosition.y);
    }
        
    if (up && !down) {
        camera.position.translateUnsafely(0, -deltaTime, 0);
    
    } else if (!up && down)
        camera.position.translateUnsafely(0, deltaTime, 0);
    
    if (rotateUp && !rotateDown) {
        camera.rotateUnsafely(-deltaTime, 0, 0);
        
    } else if (!rotateUp && rotateDown) 
        camera.rotateUnsafely(deltaTime, 0, 0);
    
    if (rotateLeft && !rotateRight) {
        camera.rotateUnsafely(0, deltaTime, 0);
        
    } else if (!rotateLeft && rotateRight) 
        camera.rotateUnsafely(0, -deltaTime, 0);
    
    if (rotateWeirdLeft && !rotateWeirdRight) {
        camera.rotateUnsafely(0, 0, deltaTime);
        
    } else if (!rotateWeirdLeft && rotateWeirdRight) 
        camera.rotateUnsafely(0, 0, -deltaTime);
        
        
    cube.rotate(0, deltaTime, deltaTime, true);
    cube2.normalize();
    cube2.width = 2;
    cube2.scaleUnsafely(Math.cos(elapsedTime / 3), true);
    
    lastTime = Date.now();
}

let canvas = new CanvasHandler.CanvasHandler(document.getElementById("canvi"));
/**
 * The primary loop for the rendering of this pseudo-game.
 */
function render() {
    canvas.refreshCanvasUnsafely(window.innerWidth, window.innerHeight);

    canvas.clearCanvas();
    WireFrameRenderer.renderCuboid(canvas, camera, cube, 0, 0, 0, 255);
    WireFrameRenderer.renderCuboid(canvas, camera, cube2, 12, 0, 55, 255);
    
    canvas.updateCanvas();
}

/**
 * Sets the new amount of calculations per second and resets the loop interval.
 *
 * Will not run if newCPS is less than or equal to 0.
 * Only sets the calculation speed if need be.
 *
 * @param {number} newCPS The new amount of calculations per second to be set.
 */
function setCPS(newCPS) {
    if (newCPS <= 0 || newCPS === calculationsPerSecond)
       return; 
   
    window.clearInterval(gameLoopRunner);
    calculationsPerSecond = newCPS;
    gameLoopRunner = window.setInterval(run, 1000 / newCPS);
}

/**
 * Sets the new amount of calculations per second and resets the loop interval.
 *
 * WARNING!: does not account for faulty input of any kind.
 * Only sets the calculation speed if need be.
 *
 * @param {number} newCPS The new amount of calculations per second to be set.
 */
function setCPSUnsafely(newCPS) {
    if (newCPS === calculationsPerSecond)
      return; 
    
    window.clearInterval(gameLoopRunner);
    calculationsPerSecond = newCPS;
    gameLoopRunner = window.setInterval(run, 1000 / newCPS);
}

/**
 * Sets the new amount of frames per second and resets the loop interval.
 *
 * Will not run if newFPS is less than or equal to 0.
 * Only sets the frame speed if need be.
 *
 * @param {number} newFPS The new amount of frames per second to be set.
 */
function setFPS(newFPS) {
   if (newFPS <= 0 || newFPS === framesPerSecond)
      return;
   
   window.clearInterval(renderLoopRunner);
   framesPerSecond = newFPS;
   renderLoopRunner = window.setInterval(render, 1000 / newFPS);
}

/**
 * Sets the new amount of frames per second and resets the loop interval.
 *
 * WARNING!: does not account for faulty input of any kind.
 * Only sets the frame speed if need be.
 *
 * @param {number} newFPS The new amount of frames per second to be set.
 */
function setFPSUnsafely(newFPS) {
   if (newFPS === framesPerSecond)
      return;
   
   window.clearInterval(renderLoopRunner);
   framesPerSecond = newFPS;
   renderLoopRunner = window.setInterval(render, 1000 / newFPS);
}

window.addEventListener('keydown', function(e) {
    let key = e.key;
    
    if (key === 'w')
        forward = true;
    
    if (key === 's')
        backward = true;
    
    if (key === 'a')
        left = true;
    
    if (key === 'd')
        right = true;
    
    if (key === ' ')
        up = true;
    
    if (key === 'Shift')
        down = true;
    
    if (key === 'ArrowLeft')
        rotateLeft = true;

    if (key === 'ArrowRight')
        rotateRight = true;
    
    if (key === 'ArrowUp')
        rotateUp = true;
    
    if (key === 'ArrowDown')
        rotateDown = true;
    
    if (key == 'q')
        rotateWeirdLeft = true;
    
    if (key === 'e')
        rotateWeirdRight = true; 
    
    if (key === 'r') {
        camera.rotation.x = 0;
        camera.rotation.z = 0;
    }
});

window.addEventListener('keyup', function(e) {
    let key = e.key;
    
    if (key === 'w')
        forward = false;
    
    if (key === 's')
        backward = false;
    
    if (key === 'a')
        left = false;
    
    if (key === 'd')
        right = false;
    
    if (key === ' ')
        up = false;
    
    if (key === 'Shift')
        down = false;
    
    if (key === 'ArrowLeft')
        rotateLeft = false;

    if (key === 'ArrowRight')
        rotateRight = false;
    
    if (key === 'ArrowUp')
        rotateUp = false;
    
    if (key === 'ArrowDown')
        rotateDown = false;
    
    if (key == 'q')
        rotateWeirdLeft = false;
    
    if (key === 'e')
        rotateWeirdRight = false; 
});