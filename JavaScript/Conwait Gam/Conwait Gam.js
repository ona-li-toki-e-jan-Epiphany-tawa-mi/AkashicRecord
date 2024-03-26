const canvas = document.getElementById("canvi");
const canvasContext = canvas.getContext("2d");

/** 
 * Stores easily configurable data about the game.
 */
const gameConfig = {
   width: 1000,
   height: 20,
   
   ticksPerSecond: 10,
   framesPerSecond: 30
};

/**
 * A game tile, live or dead.
 *
 * @param {number} x The x position of the tile.
 * @param {number} y The y position of the tile.
 */
function Tile(x, y) {
   this.x = x;
   this.y = y;
   this.previousAliveState = false;
   this.alive = false;
   
   /**
    * Called periodically to queue updates to the tile's state.
    */
   this.tick = function() {
      let neighbors = [
         game.getTile(this.x - 1, this.y + 1), game.getTile(this.x, this.y + 1), game.getTile(this.x + 1, this.y + 1),
         game.getTile(this.x - 1, this.y),                                       game.getTile(this.x + 1, this.y),
         game.getTile(this.x - 1, this.y - 1), game.getTile(this.x, this.y - 1), game.getTile(this.x + 1, this.y - 1)
      ];
      
      let aliveNeighbors = 0;
      for (let i = 0; i < 8; i++) 
         if (neighbors[i] !== null && neighbors[i].previousAliveState)
            aliveNeighbors++;
      
      if (this.previousAliveState) {
         if (aliveNeighbors !== 2 && aliveNeighbors !== 3)
            this.alive = false;
         
      } else 
         if (aliveNeighbors === 3)
            this.alive = true;
   };
   
   /**
    * Called periodically to apply the updates to the tile's state.
    */
   this.updateState = function() {
      this.previousAliveState = this.alive;
   };
   
   /**
    * Draws the tile to the screen.
    *
    * @param {number} screenWidth The width of the canvas.
    * @param {number} screenHeight The height of the canvas.
    * @param {number} halfScreenWidth Half the width of the canvas.
    * @param {number} halfScreenHeight Half the height of the canvas.
    * @param {number} halfGameWidth Half the width of the game board.
    * @param {number} halfGameHeight Half the height of the game board.
    */
   this.draw = function(screenWidth, screenHeight, halfScreenWidth, halfScreenHeight, halfGameWidth, halfGameHeight) {
      // Centers the tiles around 0, makes the camera the origin, and then scales it all by the zoom.
      let drawX = (this.x - halfGameWidth - game.camera.x) * game.camera.zoom; 
      let drawY = (this.y - halfGameHeight - game.camera.y) * game.camera.zoom;
      
      // Moves the tiles to the center of the screen.
      drawX += halfScreenWidth;
      drawY = halfScreenHeight - drawY;
      
      let nextDrawX = drawX + game.camera.zoom;
      let nextDrawY = drawX - game.camera.zoom;
      // Checks if the tile is in view.
      if ((drawX >= 0 && drawX < screenWidth && drawY >= 0 && drawY < screenHeight) || (nextDrawX >= 0 && nextDrawX < screenWidth && nextDrawY >= 0 && nextDrawY < screenHeight)) {
         // Displays the tile.
         canvasContext.strokeStyle = "#373737";
         canvasContext.strokeRect(drawX, drawY, game.camera.zoom, -game.camera.zoom);
      
         if (this.alive) {
            canvasContext.fillStyle = "#ffffff";
            canvasContext.fillRect(drawX + 0.1, drawY + 0.1, game.camera.zoom - 0.1, -game.camera.zoom - 0.1);
         }
      }
   };
};

/**
 * The instance of the game.
 */
let game = {
   tiles: [],
   
   /**
    * Gets the tile at the given position.
    *
    * @param {number} x The x position of the tile.
    * @param {number} y The y position of the tile.
    *
    * @returns The tile at the given position, or null, if it either doesn't exist or is out of bounds.
    */
   getTile: function(x, y) {
      if (x < -1 || x > gameConfig.width || y < -1 || y > gameConfig.height)
         return null;
      
      if (x === -1) {
         x = gameConfig.width - 1;
         
      } else if (x === gameConfig.width)
         x = 0;
      
      if (y === -1) {
         y = gameConfig.height - 1;
         
      } else if (y === gameConfig.height)
         y = 0;
      
      return game.tiles[x + y * gameConfig.width];
   },
   
   camera: {x: 0, y: 0, zoom: 20, speed: 200, zoomSpeed: 1},
   
   controls: {
      up: false,
      down: false,
      left: false,
      right: false,
      
      zoomIn: false,
      zoomOut: false,
      
      stop: true
   }
};

/**
 * Called when the program starts.
 */
function setup() {
   for (let y = 0; y < gameConfig.height; y++)
      for (let x = 0; x < gameConfig.width; x++)
         game.tiles.push(new Tile(x, y));
   
   runCaller = setInterval(run, 1000 / gameConfig.ticksPerSecond);
   drawCaller = setInterval(draw, 1000 / gameConfig.framesPerSecond);
}

let runCaller;
let lastTime = new Date();
/**
 * Executes game logic.
 */
function run() {
   if (!game.controls.stop) {
      for (let i = 0; i < game.tiles.length; i++)
         game.tiles[i].tick();
   
      for (let i = 0; i < game.tiles.length; i++)
         game.tiles[i].updateState();
   }
}

let drawCaller;
/**
 * Draws to the screen.
 */ 
function draw() {
   let deltaTime = (new Date() - lastTime) / 1000;
   lastTime = new Date();
   
   canvas.width = window.innerWidth;
   canvas.height = window.innerHeight;
   let width = canvas.width;
   let height = canvas.height;
   let halfWidth = width / 2;
   let halfHeight = height / 2;
   
   // Clears screen.
   canvasContext.fillStyle = "#000000";
   canvasContext.fillRect(0, 0, width, height);
   
   let halfGameWidth = gameConfig.width / 2;
   let halfGameHeight = gameConfig.height / 2;
   for (let i = 0; i < game.tiles.length; i++)
      game.tiles[i].draw(width, height, halfWidth, halfHeight, halfGameWidth, halfGameHeight);
   
   ////////////////////////////////////////////
   //                Controls                //
   ////////////////////////////////////////////
   // /\ \/
   if (game.controls.up && !game.controls.down) {
      game.camera.y += game.camera.speed * deltaTime / game.camera.zoom;
      
   } else if (!game.controls.up && game.controls.down) 
      game.camera.y -= game.camera.speed * deltaTime / game.camera.zoom;
   
   // <- ->
   if (game.controls.left && !game.controls.right) {
      game.camera.x -= game.camera.speed * deltaTime / game.camera.zoom;
      
   } else if (!game.controls.left && game.controls.right) 
      game.camera.x += game.camera.speed * deltaTime / game.camera.zoom;
   
   // zoom
   if (game.controls.zoomIn && !game.controls.zoomOut) {
      game.camera.zoom += game.camera.zoomSpeed * game.camera.zoom * deltaTime;
      
   } else if (!game.controls.zoomIn && game.controls.zoomOut)
      game.camera.zoom -= game.camera.zoomSpeed * game.camera.zoom * deltaTime;
   
   // Constrians the camera's position.
   if (game.camera.x < -gameConfig.width) {
      game.camera.x = -gameConfig.width;
      
   } else if (game.camera.x > gameConfig.width)
      game.camera.x = gameConfig.width;
   
   if (game.camera.y < -gameConfig.height) {
      game.camera.y = -gameConfig.height;
      
   } else if (game.camera.y > gameConfig.height)
      game.camera.y = gameConfig.height;
   /////////////////////////////////////////////
}

window.addEventListener("keydown", function(keydownEvent) {
   let key = keydownEvent.key;
   
   if (key === "w") {
      game.controls.up = true;
   
   } else if (key === "s") {
      game.controls.down = true;
   
   } else if (key === "a") {
      game.controls.left = true;
   
   } else if (key === "d") {
      game.controls.right = true;
   
   } else if (key === "1") {
      game.controls.zoomIn = true;
   
   } else if (key === "2") {
      game.controls.zoomOut = true;
   
   } else if (key === " ")
      game.controls.stop = !game.controls.stop;
});

window.addEventListener("keyup", function(keyupEvent) {
   let key = keyupEvent.key;
   
   if (key === "w") {
      game.controls.up = false;
   
   } else if (key === "s") {
      game.controls.down = false;
   
   } else if (key === "a") {
      game.controls.left = false;
   
   } else if (key === "d") {
      game.controls.right = false;
   
   } else if (key === "1") {
      game.controls.zoomIn = false;
   
   } else if (key === "2") 
      game.controls.zoomOut = false;
});

window.addEventListener("mousedown", function(mousedownEvent) {
   if (mousedownEvent.which === 1 && game.camera.zoom !== 0) {
      let canvasPosition = canvas.getBoundingClientRect();
      let mouseX = mousedownEvent.clientX - canvasPosition.x;
      let mouseY = mousedownEvent.clientY - canvasPosition.y;
      
      mouseX -= canvas.width / 2;
      mouseY = canvas.height / 2 - mouseY;
      
      mouseX = mouseX / game.camera.zoom + gameConfig.width / 2 + game.camera.x;
      mouseY = mouseY / game.camera.zoom + gameConfig.height / 2 + game.camera.y;
      
      let selectedTile = game.getTile(Math.floor(mouseX), Math.floor(mouseY));
      
      if (selectedTile !== null) {
         selectedTile.alive = !selectedTile.alive;
         selectedTile.previousAliveState = selectedTile.alive;
      }
   }
});

setup();