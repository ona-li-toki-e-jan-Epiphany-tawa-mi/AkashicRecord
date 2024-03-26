const canvas = document.getElementById("canvi");
const canvasContext = canvas.getContext("2d");

/**
 * Used to test for collisions between objects;
 */
let CollisionBox = {
   /**
    * Creates a circular collision box.
    *
    * @param {number} radius The radius of the collision box.
    *
    * @returns {object} The new collision box.
    */
   CircularCollisionBox: function(radius) {
      this.collisionType = 'circular';
      this.x = 0;
      this.y = 0;
      this.radius = radius;
   }
};

/**
 * Checks if a list contains an element.
 *
 * @param {object} list The list to look through.
 * @param {T} element The element to look for.
 *
 * @returns {boolean} Whether or not the list contains the element.
 */
function contains(list, element) {
   for (let i = 0; i < list.length; i++) 
      if (list[i] === element)
         return true;
   
   return false;
}

/**
 * Represents a physical object.
 */
let PhysicsObject = {
   count: 0,
   
   /**
    * Creates a new physics object.
    *
    * @param {number} x The x position of the object.
    * @param {number} y The y position of the object.
    * @param {number} mass The mass of the object.
    * @param {boolean} isImmovable Whether or not the object cannot be moved.
    * @param {object} collisionBox The collision box used for testing collisions between objects.
    */
   PhysicsObject: function(x, y, mass, isImmovable, collisionBox, image) {
      this.id = PhysicsObject.count;
      PhysicsObject.count++;
      
      this.x = x;
      this.y = y;
      this.xVelocity = 0;
      this.yVelocity = 0;
      
      this.mass = mass;
      this.immovable = isImmovable;
      
      this.collisionBox = collisionBox;
      
      this.image = image;
   },
   
   /**
    * Gets the angle between to objects from the positive x-axis, in radians.
    *
    * @param {object} firstObject The first physical object, acting as the origin.
    * @param {object} secondObject The second physical object.
    *
    * @returns {number} The angle between the two objects, in radians.
    */
   angleBetween: function(firstObject, secondObject) {
      let deltaX = secondObject.x - firstObject.x;
      let deltaY = secondObject.y - firstObject.y;
      
      if (deltaX === 0 && deltaY === 0) 
         return undefined;
      
      if (deltaX === 0) 
         if (deltaY > 0) {
            return Math.PI / 2;
            
         } else 
            return Math.PI * (3 / 2);
      
      let angle = Math.atan(Math.abs(deltaY / deltaX));
      
      if (deltaX < 0) {
         if (deltaY >= 0) {
            return Math.PI - angle;
            
         } else 
            return angle + Math.PI;
         
      } else 
         if (deltaY < 0)
            return 2 * Math.PI - angle;
      
      return angle;
   },
   
   /**
    * Gets the nearest object to a location, ignoring those in the blacklist.
    *
    * @param {number} fromX The x position to find the nearest object from.
    * @param {number} fromY The y position to find the nearest object from.
    * @param {object} blacklist A list of objects to ignore.
    *
    * @returns {object} The nearest object to the location, or null, if it finds nothing.
    */
   findNearestObject: function(fromX, fromY, blacklist) {
      let smallestDistanceSquared = Infinity;
      let nearestObject = null;
      
      for (let i = 0; i < game.objects.length; i++) 
         if (!contains(blacklist, game.objects[i])) {
            let distanceSquared = Math.pow(game.objects[i].x - fromX, 2) + Math.pow(game.objects[i].y - fromY, 2);
            
            if (distanceSquared < smallestDistanceSquared) {
               smallestDistanceSquared = distanceSquared;
               nearestObject = game.objects[i];
            }
         }
      
      return nearestObject;
   }
};

// An instance of the game.
let game = {
   // The objects in physical space.
   objects: [
      new PhysicsObject.PhysicsObject(0, 0, 5.97237E24, false, new CollisionBox.CircularCollisionBox(6371), document.getElementById("earth")),
      new PhysicsObject.PhysicsObject(0, 98027, 7.342E22, false, new CollisionBox.CircularCollisionBox(1737.4), document.getElementById("moon"))
   ],
   
   // The camera, through which we can see.
   camera: {x: 0, y: 0, zoom: 0.002, rotation: 0, following: null}
};

/**
 * Called when the program starts.
 */
function setup() {
   game.objects[1].xVelocity = 1022 * 64;
   
   logicCaller = setInterval(logicLoop, 1000 / 20);
   physicsCaller = setInterval(physicsLoop, 1000 / 60);
   drawCaller = setInterval(drawLoop, 1000 / 30);
}

let logicLastTime = new Date();
let logicCaller;
/**
 * Executes game logic.
 */
function logicLoop() {
   let deltaTime = (new Date() - logicLastTime) / 1000;
   logicLastTime = new Date();
   
   for (let i = 0; i < game.objects.length; i++)
      if (game.objects[i].mass !== 0)
         for (let k = 0; k < game.objects.length; k++)
            // Applies gravity.
            if (i !== k && !game.objects[k].immovable && game.objects[k].mass !== 0) {
               let distanceBetweenSquared = Math.pow(game.objects[i].x - game.objects[k].x, 2) + Math.pow(game.objects[i].y - game.objects[k].y, 2);
               let forceOfGravity = 6.67408E-11 * game.objects[i].mass * game.objects[k].mass / distanceBetweenSquared;
               let acceleration = forceOfGravity / game.objects[k].mass;
               
               let first2SecondX = game.objects[i].x - game.objects[k].x;
               let first2SecondY = game.objects[i].y - game.objects[k].y;
               
               let first2SecondLength = Math.sqrt(first2SecondX * first2SecondX + first2SecondY * first2SecondY);
               first2SecondX /= first2SecondLength;
               first2SecondY /= first2SecondLength;
               
               first2SecondX *= acceleration;
               first2SecondY *= acceleration;
               
               game.objects[k].xVelocity += first2SecondX * deltaTime;
               game.objects[k].yVelocity += first2SecondY * deltaTime;
            }
}

let physicsLastTime = new Date();
let physicsCaller;
/**
 * Updates game physics.
 */
function physicsLoop() {
   let deltaTime = (new Date() - physicsLastTime) / 1000;
   physicsLastTime = new Date();
   
   for (let i = 0; i < game.objects.length; i++) {
      // Moves object according to their velocity.
      game.objects[i].x += game.objects[i].xVelocity * deltaTime;
      game.objects[i].y += game.objects[i].yVelocity * deltaTime;
      
      // Handles collisions.
      if (!game.objects[i].immovable && game.objects[i].mass !== 0 && game.objects[i].collisionBox != null)
         for (let k = 0; k < game.objects.length; k++) 
            if (i !== k && game.objects[i].collisionBox != null) {
               let distanceBetween = Math.sqrt(Math.pow(game.objects[i].x - game.objects[k].x, 2) + Math.pow(game.objects[i].y - game.objects[k].y, 2));
               
               if (distanceBetween <= game.objects[i].collisionBox.radius + game.objects[k].collisionBox.radius) {
                  let encroachedDistance = game.objects[i].collisionBox.radius + game.objects[k].collisionBox.radius - distanceBetween;
                  
                  let firstFromSecondX = game.objects[i].x - game.objects[k].x;
                  let firstFromSecondY = game.objects[i].y - game.objects[k].y;
                  
                  let firstFromSecondLength = Math.sqrt(firstFromSecondX * firstFromSecondX + firstFromSecondY * firstFromSecondY);
                  firstFromSecondX /= firstFromSecondLength;
                  firstFromSecondY /= firstFromSecondLength;
                  
                  firstFromSecondX *= encroachedDistance;
                  firstFromSecondY *= encroachedDistance;
                  
                  game.objects[i].x += firstFromSecondX;
                  game.objects[i].y += firstFromSecondY;
               }
            }
                 
      // Corrects the position of an object's collision box.
      if (game.objects[i].collisionBox !== null) {
         game.objects[i].collisionBox.x = game.objects[i].x;
         game.objects[i].collisionBox.y = game.objects[i].y;
      }
   }
}

let drawCaller;
/**
 * Draws to the screen.
 */ 
function drawLoop() {
   canvas.width = window.innerWidth;
   canvas.height = window.innerHeight;
   let width = canvas.width;
   let height = canvas.height;
   let halfWidth = width / 2;
   let halfHeight = height / 2;
   
   // Clears screen.
   canvasContext.fillStyle = "#000000";
   canvasContext.fillRect(0, 0, width, height);
   
   if (game.camera.following !== null && game.objects[game.camera.following] !== undefined) {
      // Makes the camera follow stuff.
      let followedObject = game.objects[game.camera.following];
      
      game.camera.x = followedObject.x;
      game.camera.y = followedObject.y;
      
      let nearestObject = PhysicsObject.findNearestObject(game.camera.x, game.camera.y, [followedObject]);
      
      if (nearestObject !== null) {
         game.camera.rotation = -PhysicsObject.angleBetween(followedObject, nearestObject) - Math.PI / 2;
      }
      
   } else
      game.camera.rotation = 0;
   
   // Draws the physical objects.
   canvasContext.fillStyle = "#ffffff";
   for (let i = 0; i < game.objects.length; i++) {
      let object = game.objects[i]; 
      
      if (object.collisionBox !== null) {
         let drawX = (object.x - game.camera.x) * game.camera.zoom;
         let drawY = (object.y - game.camera.y) * game.camera.zoom;
         
         let rotatedDrawX = drawX * Math.cos(game.camera.rotation) - drawY * Math.sin(game.camera.rotation);
         let rotatedDrawY = drawX * Math.sin(game.camera.rotation) + drawY * Math.cos(game.camera.rotation);
         
         rotatedDrawX += halfWidth;
         rotatedDrawY = halfHeight - rotatedDrawY;
         
         let scaledRadius = object.collisionBox.radius * game.camera.zoom;
         
         if (object.image !== null) {
            canvasContext.drawImage(object.image, rotatedDrawX - scaledRadius / 2, rotatedDrawY + scaledRadius / 2, scaledRadius, scaledRadius)
            
         } else {
            canvasContext.beginPath();
            canvasContext.arc(rotatedDrawX, rotatedDrawY, scaledRadius, 0, 2 * Math.PI);
            canvasContext.fill();
         }
      }
   }
}

setup();