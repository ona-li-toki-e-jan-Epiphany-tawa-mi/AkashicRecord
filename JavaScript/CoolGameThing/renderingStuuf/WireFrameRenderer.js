import * as Vector2d from '/coordinations/Vector2d.js';
import * as Cuboid from '/coordinations/shapes/Cuboid.js';
import * as Vector3d from '/coordinations/Vector3d.js';

/**
 * Converts a 3d coordinate in pube space (normalized space) into a 2d coordinate in screen space.
 *
 * Will use the 2nd side of the or if x_or_positionVector is a 3d vector.
 * For internal use only.
 *
 * @param {object} camera The camera, acting as a pivot.
 * @param {number/object} x_or_positionVector The x coordinate in pube space. Or, a 3d vector representing the position in pube space.
 * @param {number/number} y_or_halfWidth The y coordinate in pube space. Or, half of the width of the screen.
 * @param {number/number} z_or_halfHeight The z coordinate in pube space. Or, half of the height of the screen.
 * @param {number} halfWidth Half of the width of the screen.
 * @param {number} halfHeight Half of the height of the screen.
 *
 * @returns A 2d vector representing the equivalent position in screen space.
 */
function pubeSpaceToScreenSpace(x_or_positionVector, y_or_halfWidth, z_or_halfHeight, halfWidth, halfHeight) {
    let transformedPosition;
    
    if (x_or_positionVector.type === 'vector3d') {
        transformedPosition = x_or_positionVector;
        
        halfWidth = y_or_halfWidth;
        halfHeight = z_or_halfHeight;
        
    } else 
        transformedPosition = new Vector3d.Vector3d(x_or_positionVector, y_or_halfWidth, z_or_halfHeight);
    
    let newX = transformedPosition.x / transformedPosition.z * halfWidth + halfWidth;
    let newY = transformedPosition.y / transformedPosition.z * halfHeight + halfHeight;
    
    return new Vector2d.Vector2d(newX, newY);
}

/**
 * Rotates and translates pube space so that the camera is the origin, and that the camera's rotation becomes the basis vectors..
 *
 * Will use the 2nd side of the or if x_or_positionVector is a 3d vector.
 * For internal use only.
 *
 * @param {object} camera The camera, acting as a pivot.
 * @param {number/object} x_or_positionVector The x coordinate in pube space. Or, a 3d vector representing the position in pube space.
 * @param {number} y The y coordinate in pube space.
 * @param {number} z The z coordinate in pube space.
 *
 * @param {object} A Vector3d that is conformed to the camera.
 */
function conformPubeSpaceToCamera(camera, x_or_positionVector, y, z) {
    if (x_or_positionVector.type === 'vector3d') {
        return Vector3d.subtractUnsafely(x_or_positionVector, camera.position).rotateUnsafely(camera.rotation);
        
    } else 
        return new Vector3d.Vector3d(x_or_positionVector, y, z).subtractUnsafely(camera.position).rotateUnsafely(camera.rotation);
}

/**
 * Renders a cuboid as a wireframe.
 *
 * Fails if canvasHandler is not a CanvasHandler.
 * Fails if camera is not a Camera3d
 * Fails if cuboid is not a Cuboid
 * Constrains red, green, blue, and alpha to be between 0 and 255.
 * Sets red, green, and blue to 0 if they are not numbers.
 * Sets alpha to 255 if it's not a number.
 *
 * @param {object} canvasHandler The CanvasHandler to draw with.
 * @param {object} camera The camera, acting as a pivot.
 * @param {object} cuboid the Cuboid to render.
 * @param {number} red The value of the pixel's red channel to be set. Gets constrained to: 0 <= red <= 255.
 * @param {number} green The value of the pixel's green channel to be set. Gets constrained to: 0 <= green <= 255.
 * @param {number} blue The value of the pixel's blue channel to be set. Gets constrained to: 0 <= blue <= 255.
 * @param {number} alpha The value of the pixel's alpha channel to be set. Gets constrained to: 0 <= alpha <= 255.
 */
export function renderCuboid(canvasHandler, camera, cuboid, red, green, blue, alpha) {
    if (canvasHandler.type !== 'canvashandler' || camera.type !== 'camera3d' || cuboid.type !== 'cuboid')
        return;
    
    if (typeof red === 'number') {
        if (red < 0)
            red = 0;
        else if (red > 255)
            red = 255;
    } else 
        red = 0;
    
    if (typeof green === 'number') {
        if (green < 0)
            green = 0;
        else if (green > 255)
            green = 255;
    } else
        green = 0;
    
    if (typeof blue === 'number') {
        if (blue < 0)
            blue = 0;
        else if (blue > 255)
            blue = 255;
    } else
        blue = 0;
    
    if (typeof alpha === 'number') {
        if (alpha < 0)
            alpha = 0;
        else if (alpha > 255)
            alpha = 255;
    } else
        alpha = 255;
    
    for (let i = 0; i < Cuboid.lineVerticeIndexes.length; i++) {
        let point1 = conformPubeSpaceToCamera(camera, cuboid.vertices[Cuboid.lineVerticeIndexes[i].a].x, cuboid.vertices[Cuboid.lineVerticeIndexes[i].a].y, cuboid.vertices[Cuboid.lineVerticeIndexes[i].a].z); 
        let point2 = conformPubeSpaceToCamera(camera, cuboid.vertices[Cuboid.lineVerticeIndexes[i].b].x, cuboid.vertices[Cuboid.lineVerticeIndexes[i].b].y, cuboid.vertices[Cuboid.lineVerticeIndexes[i].b].z); 
        
        // Clipping.
        if (point1.z > 0 && point2.z > 0)
            if ((Math.abs(point1.x / point1.z) <= 1 && Math.abs(point1.y / point1.z) <= 1) || (Math.abs(point2.x / point2.z) <= 1 && Math.abs(point2.y / point2.z) <= 1)) {
                point1 = pubeSpaceToScreenSpace(point1, canvasHandler.halfWidth, canvasHandler.halfHeight);
                point2 = pubeSpaceToScreenSpace(point2, canvasHandler.halfWidth, canvasHandler.halfHeight);
            
                canvasHandler.drawLineUnsafely(point1.x, point1.y, point2.x, point2.y, red, green, blue, alpha);
            }
    }
}

/**
 * Renders a cuboid as a wireframe.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} canvasHandler The CanvasHandler to draw with.
 * @param {object} camera The camera, acting as a pivot.
 * @param {object} cuboid the Cuboid to render.
 * @param {number} red The value of the pixel's red channel to be set. Gets constrained to: 0 <= red <= 255.
 * @param {number} green The value of the pixel's green channel to be set. Gets constrained to: 0 <= green <= 255.
 * @param {number} blue The value of the pixel's blue channel to be set. Gets constrained to: 0 <= blue <= 255.
 * @param {number} alpha The value of the pixel's alpha channel to be set. Gets constrained to: 0 <= alpha <= 255.
 */
export function renderCuboidUnsafely(canvasHandler, camera, cuboid, red, green, blue, alpha) {
    for (let i = 0; i < Cuboid.lineVerticeIndexes.length; i++) {
        let vector1 = pubeSpaceToScreenSpace(camera, cuboid.vertices[Cuboid.lineVerticeIndexes[i].a].x, cuboid.vertices[Cuboid.lineVerticeIndexes[i].a].y, cuboid.vertices[Cuboid.lineVerticeIndexes[i].a].z, canvasHandler.halfWidth, canvasHandler.halfHeight);
        let vector2 = pubeSpaceToScreenSpace(camera, cuboid.vertices[Cuboid.lineVerticeIndexes[i].b].x, cuboid.vertices[Cuboid.lineVerticeIndexes[i].b].y, cuboid.vertices[Cuboid.lineVerticeIndexes[i].b].z, canvasHandler.halfWidth, canvasHandler.halfHeight);
            
        canvasHandler.drawLineUnsafely(vector1.x, vector1.y, vector2.x, vector2.y, red, green, blue, alpha)
    }
}