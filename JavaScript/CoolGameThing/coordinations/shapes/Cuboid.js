import * as Vector3d from '/coordinations/Vector3d.js';
import * as cMath from '/Meth.js';

/**
 * @class
 *
 * Creates a reperesentation of a cubiod, which is like a 3d rectangle.
 *
 * @param {number} width The width of the cuboid (x.)
 * @param {number} length The length of the cuboid (y.)
 * @param {number} height The height of the cuboid (z.)
 * @param {number/object} x_or_positionVector The x position of the cuboid, or its position as a 3d vector.
 * @param {number/object} y_or_rotationVector The y position of the cuboid, or its rotation as a 2d vector.
 * @param {number} z The z position of the cuboid.
 * @param {number} xRot How much the cuboid is rotated away from the positive x axis.
 * @param {number} yRot How much the cuboid is rotated away from the positive y axis.
 * @param {number} zRot How much the cuboid is rotated away from the positive z axis.
 *
 * @returns A new cuboid.
 */
export function Cuboid(width, length, height, x_or_positionVector, y_or_rotationVector, z, xRot, yRot, zRot) {
    this.type = 'cuboid';
    
    if (typeof length !== 'number')
        length = 0;
    
    if (typeof width !== 'number')
        width = 0;
    
    if (typeof height !== 'number')
        height = 0;
    
    this.length = length;
    this.width = width;
    this.height = height;
    
    // Cuboid position and rotation from vectors.
    if (x_or_positionVector.type === 'vector3d') {
        if (y_or_rotationVector.type !== 'vector3d')
            y_or_rotationVector = new Vector3d.Vector3d(0, 0, 0);

        this.position = x_or_positionVector;
        this.rotation = y_or_rotationVector;
        
    // Cuboid position and rotation from raw numbers.
    } else {
        let x = x_or_positionVector;
        let y = y_or_rotationVector;
        
        if (typeof x !== 'number')
            x = 0;
    
        if (typeof y !== 'number')
            y = 0;
    
        if (typeof z !== 'number')
            z = 0;
        
        if (typeof xRot !== 'number')
            xRot = 0;
        
        if (typeof yRot !== 'number')
            zRot = 0;
    
        if (typeof zRot !== 'number')
            zRot = 0;
    
        this.position = new Vector3d.Vector3d(x, y, z);
        this.rotation = new Vector3d.Vector3d(xRot, yRot, zRot);
    }
    
    this.vertices = [];
    
    /**
     * @function
     *
     * Updates the vertices of the cube to it's current position and rotation.
     */ 
    this.updateVertices = function() {
        this.vertices = [];
        
        let halfWidth = this.width / 2;
        let halfLength = this.length / 2;
        let halfHeight = this.height / 2;
        
        this.vertices[0] = new Vector3d.Vector3d(halfWidth, halfLength, halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[1] = new Vector3d.Vector3d(-halfWidth, -halfLength, -halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[2] = new Vector3d.Vector3d(halfWidth, -halfLength, -halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[3] = new Vector3d.Vector3d(halfWidth, halfLength, -halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[4] = new Vector3d.Vector3d(-halfWidth, halfLength, halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[5] = new Vector3d.Vector3d(-halfWidth, -halfLength, halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[6] = new Vector3d.Vector3d(-halfWidth, halfLength, -halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
        this.vertices[7] = new Vector3d.Vector3d(halfWidth, -halfLength, halfHeight).rotateUnsafely(this.rotation).addUnsafely(this.position);
    }
    
    this.updateVertices();
    
    /**
     * @function
     *
     * Scales the cube's size by scalar, updating its vertices if asked to.
     *
     * Does nothing if scalar is not a number (scalar of 1.)
     * Sets updateVertices to true if it's not a boolean.
     * WARNING!: this function will mutate the cuboid.
     *
     * @param {number} scalar The amount to scale the size of the cube by.
     * @param {boolean} updateVertices Whether or not to update the cubes vertices after the scaling.
     *
     * @returns {object} Itself, for chaining.
     */
    this.scale = function(scalar, updateVertices) {
        if (typeof scalar !== 'number') 
            return this;
        
        if (typeof updateVertices !== 'boolean')
            updateVertices = true;
        
        this.width *= scalar;
        this.length *= scalar;
        this.height *= scalar;
        
        if (updateVertices)
            this.updateVertices();
        
        return this;
    }
    
    /**
     * @function
     *
     * Scales the cube's size by scalar, updating its vertices if asked to.
     *
     * Does nothing if scalar is not a number (scalar of 1.)
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the cuboid.
     *
     * @param {number} scalar The amount to scale the size of the cube by.
     * @param {boolean} updateVertices Whether or not to update the cubes vertices after the scaling.
     *
     * @returns {object} Itself, for chaining.
     */
    this.scaleUnsafely = function(scalar, updateVertices) {
        if (typeof scalar !== 'number') 
            return this;
        
        this.width *= scalar;
        this.length *= scalar;
        this.height *= scalar;
        
        if (updateVertices)
            this.updateVertices();
        
        return this;
    }
    
    /**
     * @function
     *
     * Normalizes the size of the cuboid, updating its vertices if asked to.
     *
     * Sets updateVertices to true if it's not a boolean.
     * WARNING!: this function will mutate the cuboid.
     *
     * @param {boolean} updateVertices Whether or not to update the cubes vertices after the scaling.
     *
     * @returns {object} Itself, for chaining.
     */
    this.normalize = function(updateVertices) {
        if (typeof updateVertices !== 'boolean')
            updateVertices = true;
        
        this.width = 1;
        this.length = 1;
        this.height = 1;
        
        if (updateVertices)
            this.updateVertices();
        
        return this;
    }
    
    /**
     * @function
     *
     * Normalizes the size of the cuboid, updating its vertices if asked to.
     *
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the cuboid.
     *
     * @param {boolean} updateVertices Whether or not to update the cubes vertices after the scaling.
     *
     * @returns {object} Itself, for chaining.
     */
    this.normalizeUnsafely = function(updateVertices) {
        this.width = 1;
        this.length = 1;
        this.height = 1;
        
        if (updateVertices)
            this.updateVertices();
        
        return this;
    }
    
    /**
     * @function
     *
     * Rotates the cuboid along the axes, updating its vertices if asked to.
     *
     * Uses the first parameter to get the rotation if it's a vector.
     * Sets xRot_radians, yRot_radians, and zRot_radians to 0 if they are not numbers.
     * Sets updateVertices to true if it's not a boolean.
     * WARNING!: this function will mutate the cuboid.
     *
     * @param {number/object} xRot_radians__or__rotationVector The amount to rotate the cuboid from the x axis by, in radians. Or, a vector describing all three rotation measures.
     * @param {number} yRot_radians The amount to rotate the cuboid from the y axis by, in radians.
     * @param {number} zRot_radians The amount to rotate the cuboid from the z axis by, in radians.
     * @param {boolean} updateVertices Whether or not to update the cubes vertices after the rotation.
     *
     * @returns {object} Itself, for chaining.
     */
    this.rotate = function(xRot_radians__or__rotationVector, yRot_radians, zRot_radians, updateVertices) {
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
        
        if (typeof updateVertices !== 'boolean')
            updateVertices = true;
        
        this.rotation.translateUnsafely(xRot_radians, yRot_radians, zRot_radians);
        
        let pi2 = 2 * Math.PI;
        
        if (this.rotation.x < 0 || this.rotation.x > pi2)
            this.rotation.x = cMath.fmod(this.rotation.x, pi2, 11);
        
        if (this.rotation.y < 0 || this.rotation.y > pi2)
            this.rotation.y = cMath.fmod(this.rotation.y, pi2, 11);
        
        if (this.rotation.z < 0 || this.rotation.z > pi2)
            this.rotation.z = cMath.fmod(this.rotation.z, pi2, 11);
        
        if (updateVertices)
            this.updateVertices();
        
        return this;
    }
    
    /**
     * @function
     *
     * Rotates the cuboid along the axes, updating its vertices if asked to.
     *
     * Uses the first parameter to get the rotation if it's a vector.
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the cuboid.
     *
     * @param {number/object} xRot_radians__or__rotationVector The amount to rotate the cuboid from the x axis by, in radians. Or, a vector describing all three rotation measures.
     * @param {number} yRot_radians The amount to rotate the cuboid from the y axis by, in radians.
     * @param {number} zRot_radians The amount to rotate the cuboid from the z axis by, in radians.
     * @param {boolean} updateVertices Whether or not to update the cubes vertices after the rotation.
     *
     * @returns {object} Itself, for chaining.
     */
    this.rotateUnsafely = function(xRot_radians__or__rotationVector, yRot_radians, zRot_radians, updateVertices) {
        let xRot_radians;
        
        if (xRot_radians__or__rotationVector.type === 'vector3d') {
            xRot_radians = xRot_radians__or__rotationVector.x;
            yRot_radians = xRot_radians__or__rotationVector.y;
            zRot_radians = xRot_radians__or__rotationVector.z;
            
        } else 
            xRot_radians = xRot_radians__or__rotationVector;
        
        this.rotation.translateUnsafely(xRot_radians, yRot_radians, zRot_radians);
        
        let pi2 = 2 * Math.PI;
        
        if (this.rotation.x < 0 || this.rotation.x > pi2)
            this.rotation.x = cMath.fmod(this.rotation.x, pi2, 11);
        
        if (this.rotation.y < 0 || this.rotation.y > pi2)
            this.rotation.y = cMath.fmod(this.rotation.y, pi2, 11);
        
        if (this.rotation.z < 0 || this.rotation.z > pi2)
            this.rotation.z = cMath.fmod(this.rotation.z, pi2, 11);
        
        if (updateVertices)
            this.updateVertices();
        
        return this;
    }
    
    /**
     * @function
     *
     * Returns the cuboid as a string.
     *
     * @returns {string} The cuboid as a string.
     */
    this.toString = function() {
        return "{type: " + this.type + ", position: " + this.position.toString() + ", rotation: " + this.rotation.toString() + "}";
    }
}

/**
 * Scales the cube's size by scalar.
 *
 * Fails if cuboid is not a cuboid.
 * Does nothing if scalar is not a number (scalar of 1.)
 *
 * @param {object} The cuboid to scale.
 * @param {number} scalar The amount to scale the size of the cube by.
 *
 * @returns {object} A new cuboid that is the dilation of cuboid.
 */
export function scale(cuboid, scalar) {
    if (cuboid.type !== 'cuboid')
        return;
    
    if (typeof scalar !== 'number') 
        return new Cuboid(cuboid.width, cuboid.length, cuboid.height, cuboid.position, cuboid.rotation);
        
    if (typeof updateVertices !== 'boolean')
        updateVertices = true;
    
    newWidth = cuboid.width * scalar;
    newLength = cuboid.length * scalar;
    newHeight = cuboid.height * scalar;
    
    return new Cuboid(newWidth, newLength, newHeight, cuboid.position, cuboid.rotation);
}
    
/**
 * Scales the cube's size by scalar.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} The cuboid to scale.
 * @param {number} scalar The amount to scale the size of the cube by.
 *
 * @returns {object} A new cuboid that is the dilation of cuboid.
 */
export function scaleUnsafely(cuboid, scalar) {
    if (typeof scalar !== 'number') 
        return new Cuboid(cuboid.width, cuboid.length, cuboid.height, cuboid.position, cuboid.rotation);
    
    newWidth = cuboid.width * scalar;
    newLength = cuboid.length * scalar;
    newHeight = cuboid.height * scalar;
    
    return new Cuboid(newWidth, newLength, newHeight, cuboid.position, cuboid.rotation);
}
    
/**
 * Normalizes the size of the cuboid.
 *
 * Fails if cuboid is not a cuboid.
 *
 * @param {object} The cuboid to normalize.
 *
 * @returns {object} A new cuboid that is the normalized form of cuboid.
 */
export function normalize(cuboid) {
    if (cuboid.type !== 'cuboid')
        return;

    return new Cuboid(1, 1, 1, cuboid.position, cuboid.rotation);
}
    
/**
 * Normalizes the size of the cuboid.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} The cuboid to normalize.
 *
 * @returns {object} A new cuboid that is the normalized form of cuboid.
 */
export function normalizeUnsafely(cuboid) {
    return new Cuboid(1, 1, 1, cuboid.position, cuboid.rotation);
}

    
/**
 * Rotates the cuboid along the axes, updating its vertices if asked to.
 *
 * Fails if cuboid is not a cuboid.
 * Uses the second parameter to get the rotation if it's a vector.
 * Sets xRot_radians, yRot_radians, and zRot_radians to 0 if they are not numbers.
 *
 * @param {object} The cuboid to rotate.
 * @param {number/object} xRot_radians__or__rotationVector The amount to rotate the cuboid from the x axis by, in radians. Or, a vector describing all three rotation measures.
 * @param {number} yRot_radians The amount to rotate the cuboid from the y axis by, in radians.
 * @param {number} zRot_radians The amount to rotate the cuboid from the z axis by, in radians.
 *
 * @returns {object} A new cuboid that is the rotation of cuboid.
 */
export function rotate(cuboid, xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
    if (cuboid.type !== 'cuboid')
        return;
        
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
        
    let newXRot = xRot_radians + cuboid.rotation.x;
    let newYRot = yRot_radians + cuboid.rotation.y;
    let newZRot = zRot_radians + cuboid.rotation.z;
    
    let pi2 = 2 * Math.PI;
        
    if (newXRot < 0 || newXRot > pi2)
        newXRot = cMath.fmod(newXRot, pi2, 11);
        
    if (newYRot < 0 || newYRot > pi2)
        newYRot = cMath.fmod(newYRot, pi2, 11);
        
    if (newZRot < 0 || newZRot > pi2)
        newZRot = cMath.fmod(newZRot, pi2, 11);
    
    return new Cuboid(cuboid.width, cuboid.length, cuboid.height, cuboid.position.x, cuboid.position.y, cuboid.position.z, newXRot, newYRot, newZRot);
}
    
/**
 * Rotates the cuboid along the axes, updating its vertices if asked to.
 *
 * Uses the second parameter to get the rotation if it's a vector.
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} The cuboid to rotate.
 * @param {number/object} xRot_radians__or__rotationVector The amount to rotate the cuboid from the x axis by, in radians. Or, a vector describing all three rotation measures.
 * @param {number} yRot_radians The amount to rotate the cuboid from the y axis by, in radians.
 * @param {number} zRot_radians The amount to rotate the cuboid from the z axis by, in radians.
 *
 * @returns {object} A new cuboid that is the rotation of cuboid.
 */
export function rotateUnsafely(cuboid, xRot_radians__or__rotationVector, yRot_radians, zRot_radians) { 
    let xRot_radians;
        
    if (xRot_radians__or__rotationVector.type === 'vector3d') {
        xRot_radians = xRot_radians__or__rotationVector.x;
        yRot_radians = xRot_radians__or__rotationVector.y;
        zRot_radians = xRot_radians__or__rotationVector.z;
            
    } else 
        xRot_radians = xRot_radians__or__rotationVector;
        
    let newXRot = xRot_radians + cuboid.rotation.x;
    let newYRot = yRot_radians + cuboid.rotation.y;
    let newZRot = zRot_radians + cuboid.rotation.z;
    
    let pi2 = 2 * Math.PI;
        
    if (newXRot < 0 || newXRot > pi2)
        newXRot = cMath.fmod(newXRot, pi2, 11);
        
    if (newYRot < 0 || newYRot > pi2)
        newYRot = cMath.fmod(newYRot, pi2, 11);
        
    if (newZRot < 0 || newZRot > pi2)
        newZRot = cMath.fmod(newZRot, pi2, 11);
    
    return new Cuboid(cuboid.width, cuboid.length, cuboid.height, cuboid.position.x, cuboid.position.y, cuboid.position.z, newXRot, newYRot, newZRot);
}


// A look-up table for which vertices connect to which in the wire frame. 
export let lineVerticeIndexes = [
    {a: 0, b: 4}, {a: 4, b: 5}, {a: 5, b: 7}, {a: 7, b: 0},
    {a: 0, b: 3}, {a: 4, b: 6}, {a: 5, b: 1}, {a: 7, b: 2},
    {a: 3, b: 6}, {a: 6, b: 1}, {a: 1, b: 2}, {a: 2, b: 3}
];


