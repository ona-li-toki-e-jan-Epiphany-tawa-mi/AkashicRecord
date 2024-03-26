import * as Vector2d from '/coordinations/Vector2d.js';
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
 * Rotates and translates pube space so that the camera is the origin, and that the camera's rotation becomes the basis vectors.
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

export function drawTriangle(canvasHandler, camera, point1, point) {
    
}

function drawFlatTopTriangle(canvasHandler, camera, triangle) {
    
}

function drawFlatBottomTriangle(canvasHandler, camera, triangle) {
    
}
