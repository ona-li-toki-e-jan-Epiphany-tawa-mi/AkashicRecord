
import * as Vector3d from '/coordinations/Vector3d.js';
import * as cMath from '/Meth.js';

/**
 * @class
 *
 * @todo document.
 */
export function Camera3d(x_or_positionVector, y_or_rotationVector, z__or__fieldOfView_deg, xRot, yRot, zRot, fieldOfView) {
    this.type = 'camera3d';
    
    // Camera position and rotation from vectors.
    if (x_or_positionVector.type === 'vector3d') {
        let fieldOfView = z__or__fieldOfView_deg;
        
        if (y_or_rotationVector.type !== 'vector3d')
            y_or_rotationVector = new Vector3d.Vector3d(0, 0, 0);
        
        if (typeof fieldOfView !== 'number')
            fieldOfView = 70;
        
        this.position = x_or_positionVector;
        this.rotation = y_or_rotationVector;
        this.fieldOfView = fieldOfView;
        
    // Camera position and rotation from raw numbers.
    } else {
        let x = x_or_positionVector;
        let y = y_or_rotationVector;
        let z = z__or__fieldOfView_deg;
        
        if (typeof x !== 'number')
            x = 0;
    
        if (typeof y !== 'number')
            y = 0;
    
        if (typeof z !== 'number')
            z = 0;
        
        if (typeof xRot !== 'number')
            xRot = 0;
    
        if (typeof yRot !== 'number')
            yRot = 0;
    
        if (typeof zRot !== 'number')
            zRot = 0;
        
        if (typeof fieldOfView !== 'number')
            fieldOfView = 70;
    
        this.position = new Vector3d.Vector3d(x, y, z);
        this.rotation = new Vector3d.Vector3d(xRot, yRot, zRot);
        this.fieldOfView = fieldOfView;
    }
    
    /**
     * @function
     *
     * Rotates the camera along the axes.
     *
     * Uses the first parameter to get the rotation if it's a vector.
     * Sets xRot_radians, yRot_radians, and zRot_radians to 0 if they are not numbers.
     * Sets updateVertices to true if it's not a boolean.
     *
     * @param {number/object} xRot_radians__or__rotationVector The amount to rotate the camera from the x axis by, in radians. Or, a vector describing all three rotation measures.
     * @param {number} yRot_radians The amount to rotate the camera from the y axis by, in radians.
     * @param {number} zRot_radians The amount to rotate the camera from the z axis by, in radians.
     *
     * @returns {object} Itself, for chaining.
     */
    this.rotate = function(xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
        let xRot_radians;
        
        if (xRot_radians__or__rotationVector.type === 'vector3d') {
            xRot_radians = xRot_radians__or__rotationVector.x;
            yRot_radians = xRot_radians__or__rotationVector.y;
            zRot_radians = xRot_radians__or__rotationVector.z;
            
        } else {
            xRot_radians = xRot_radians__or__rotationVector;
            
            if (typeof xRot_radians !== 'number')
                xRot_radians = 0;
            
            if (typeof yRot_radians !== 'number')
                yRot_radians = 0;
            
            if (typeof zRot_radians !== 'number')
                zRot_radians = 0;
        }
        
        this.rotation.translateUnsafely(xRot_radians, yRot_radians, zRot_radians);
        
        let pi2 = 2 * Math.PI;
        
        if (this.rotation.x > pi2) {
            this.rotation.x = cMath.fmod(this.rotation.x, pi2, 11);
            
        } else if (this.rotation.x < 0)
            this.rotation.x = pi2 + cMath.fmod(this.rotation.x, pi2, 11);
        
        if (this.rotation.y > pi2) {
            this.rotation.y = cMath.fmod(this.rotation.y, pi2, 11);
            
        } else if (this.rotation.y < 0)
            this.rotation.y = pi2 + cMath.fmod(this.rotation.y, pi2, 11);
        
        if (this.rotation.z > pi2) {
            this.rotation.z = cMath.fmod(this.rotation.z, pi2, 11);
            
        } else if (this.rotation.z < 0)
            this.rotation.z = pi2 + cMath.fmod(this.rotation.z, pi2, 11);
        
        return this;
    }
    
    /**
     * @function
     *
     * Rotates the camera along the axes.
     *
     * Uses the first parameter to get the rotation if it's a vector.
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {number/object} xRot_radians__or__rotationVector The amount to rotate the camera from the x axis by, in radians. Or, a vector describing all three rotation measures.
     * @param {number} yRot_radians The amount to rotate the camera from the y axis by, in radians.
     * @param {number} zRot_radians The amount to rotate the camera from the z axis by, in radians.
     *
     * @returns {object} Itself, for chaining.
     */
    this.rotateUnsafely = function(xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
        let xRot_radians;
        
        if (xRot_radians__or__rotationVector.type === 'vector3d') {
            xRot_radians = xRot_radians__or__rotationVector.x;
            yRot_radians = xRot_radians__or__rotationVector.y;
            zRot_radians = xRot_radians__or__rotationVector.z;
            
        } else 
            xRot_radians = xRot_radians__or__rotationVector;
        
        this.rotation.translateUnsafely(xRot_radians, yRot_radians, zRot_radians);
        
        let pi2 = 2 * Math.PI;
        
        if (this.rotation.x > pi2) {
            this.rotation.x = cMath.fmod(this.rotation.x, pi2, 11);
            
        } else if (this.rotation.x < 0)
            this.rotation.x = pi2 + cMath.fmod(this.rotation.x, pi2, 11);
        
        if (this.rotation.y > pi2) {
            this.rotation.y = cMath.fmod(this.rotation.y, pi2, 11);
            
        } else if (this.rotation.y < 0)
            this.rotation.y = pi2 + cMath.fmod(this.rotation.y, pi2, 11);
        
        if (this.rotation.z > pi2) {
            this.rotation.z = cMath.fmod(this.rotation.z, pi2, 11);
            
        } else if (this.rotation.z < 0)
            this.rotation.z = pi2 + cMath.fmod(this.rotation.z, pi2, 11);
        
        return this;
    }
}
                        