/**
 * @class
 *
 * A 3 dimensional vector, loaded with vector math.
 *
 * @param {number} x The x component of the vector to be created.
 * @param {number} y The y component of the vector to be created.
 * @param {number} z The z component of the vector to be created.
 */
export function Vector3d(x, y, z) {
    this.type = 'vector3d';
    
    if (typeof x !== 'number')
        x = 0;
    
    if (typeof y !== 'number')
        y = 0;
    
    if (typeof z !== 'number')
        z = 0;
    
    this.x = x;
    this.y = y;
    this.z = z;
    
    /**
     * @function
     *
     * Translates the vector by (dx, dy, dz).
     *
     * deltaX, deltaY, and deltaZ will be set to 0 if they are not numbers.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} deltaX How much to change the x component of the vector by.
     * @param {number} deltaY How much to change the y component of the vector by.
     * @param {number} deltaZ How much to change the z component of the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.translate = function(deltaX, deltaY, deltaZ) {
        if (typeof deltaX !== 'number')
            deltaX = 0;
    
        if (typeof deltaY !== 'number')
            deltaY = 0;
    
        if (typeof deltaZ !== 'number')
            deltaZ = 0;
        
        this.x += deltaX;
        this.y += deltaY;
        this.z += deltaZ;
        
        return this;
    }
    
    /**
     * @function
     *
     * Translates the vector by (dx, dy, dz).
     *
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} deltaX How much to change the x component of the vector by.
     * @param {number} deltaY How much to change the y component of the vector by.
     * @param {number} deltaZ How much to change the z component of the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.translateUnsafely = function(deltaX, deltaY, deltaZ) {
        this.x += deltaX;
        this.y += deltaY;
        this.z += deltaZ;
        
        return this;
    }
    
    /**
     * @function
     *
     * Adds together two vectors.
     *
     * Fails if otherVector is not a vector.
     * WARNING!: this function will mutate the vector.
     *
     * @param {object} otherVector The other vector to be summed.
     *
     * @returns {object} Itself, for chaining.
     */
    this.add = function(otherVector) {
        if (otherVector.type !== 'vector3d')
            return;
        
        this.x += otherVector.x;
        this.y += otherVector.y;
        this.z += otherVector.z;
        
        return this;
    }
    
    /**
     * @function
     *
     * Adds together two vectors.
     *
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the vector.
     *
     * @param {object} otherVector The other vector to be summed.
     *
     * @returns {object} Itself, for chaining.
     */
    this.addUnsafely = function(otherVector) {
        this.x += otherVector.x;
        this.y += otherVector.y;
        this.z += otherVector.z;
        
        return this;
    }
    
    /**
     * @function
     *
     * Subtracts two vectors from eachother.
     *
     * Fails if otherVector is not a vector.
     * WARNING!: this function will mutate the vector.
     *
     * @param {object} otherVector The other vector, acting as the subtrahend.
     *
     * @returns {object} Itself, for chaining.
     */
    this.subtract = function(otherVector) {
        if (otherVector.type !== 'vector3d')
            return;
        
        this.x -= otherVector.x;
        this.y -= otherVector.y;
        this.z -= otherVector.z;
        
        return this;
    }
    
    /**
     * @function
     *
     * Subtracts two vectors from eachother.
     *
     * WARNING!: does not account for faulty input of any kind.     
     * WARNING!: this function will mutate the vector.
     *
     * @param {object} otherVector The other vector, acting as the subtrahend.
     *
     * @returns {object} Itself, for chaining.
     */
    this.subtractUnsafely = function(otherVector) {
        this.x -= otherVector.x;
        this.y -= otherVector.y;
        this.z -= otherVector.z;
        
        return this;
    }
    
    /**
     * @function
     *
     * Scales the vector by the scalar.
     *
     * Will fail if scalar is not a number.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} scalar The amount to scale the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.scale = function(scalar) {
        if (typeof scalar !== 'number')
            return;
        
        this.x *= scalar;
        this.y *= scalar;
        this.z *= scalar;
        
        return this;
    }
    
    /**
     * @function
     *
     * Scales the vector by the scalar.
     *
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} scalar The amount to scale the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.scaleUnsafely = function(scalar) {
        this.x *= scalar;
        this.y *= scalar;
        this.z *= scalar;
        
        return this;
    }
    
    /**
     * @function
     *
     * Gets the length of the vector, but leaves it squared
     *
     * @returns {number} The length of the vector squared.
     */
    this.getLengthSquared = function() {
        return this.x * this.x + this.y * this.y + this.z + this.z;
    }
    
    /**
     * @function
     *
     * Gets the length of the vector.
     *
     * @returns {number} The length of the vector.
     */
    this.getLength = function() {
        return Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z);
    }
    
    /**
     * @function
     *
     * Normalizes the vector, making its length 1.
     *
     * Does nothing if the length of the vector is 0.
     * WARNING!: this function will mutate the vector.
     *
     * @returns {object} Itself, for chaining.
     */
    this.normalize = function() {
        let length = this.getLength();
        
        if (length != 0) {
            this.x /= length;
            this.y /= length;
            this.z /= length;
        }
        
        return this;
    }
    
    /**
     * @function
     *
     * Gets the dot product of two vectors.
     *
     * Will fail if otherVector is not a vector.
     *
     * @param {object} The other vector.
     *
     * @returns {number} The cross product.
     */
    this.getDotProduct = function(otherVector) {
        if (otherVector.type !== 'vector3d')
            return;
        
        return this.x * otherVector.x + this.y * otherVector.y + this.z * otherVector.z;
    }
    
    /**
     * @function
     *
     * Gets the dot product of two vectors.
     *
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {object} The other vector.
     *
     * @returns {number} The cross product of the two vectors.
     */
    this.getDotProductUnsafely = function(otherVector) {
        return this.x * otherVector.x + this.y * otherVector.y + this.z * otherVector.z;
    }
    
    /**
     * @function
     *
     * Gets the cross product of two vectors.
     *
     * Will fail if otherVector is not a vector.
     *
     * @returns {object} The cross product of the two vectors (a vector).
     */
    this.getCrossProduct = function(otherVector) {
        if (otherVector.type !== 'vector3d')
            return;
        
        let newX = this.y * otherVector.z - this.z * otherVector.y;
        let newY = this.z * otherVector.x - this.x * otherVector.z;
        let newZ = this.x * otherVector.y - this.y * otherVector.x;
        
        return new Vector3d(newX, newY, newZ);
    }
    
    /**
     * @function
     *
     * Gets the cross product of two vectors.
     *
     * WARNING!: does not account for faulty input of any kind.
     *
     * @returns {object} The cross product (a vector).
     */
    this.getCrossProductUnsafely = function(otherVector) {
        let newX = this.y * otherVector.z - this.z * otherVector.y;
        let newY = this.z * otherVector.x - this.x * otherVector.z;
        let newZ = this.x * otherVector.y - this.y * otherVector.x;
        
        return new Vector3d(newX, newY, newZ);
    }
    
    /**
     * @function
     *
     * Rotates the vector along the x, y, and z axes.
     * Uses the first argument as the rotation angles if it is a 3d vector.
     *
     * xRot, yRot, and zRot will be set to 0 if they are not numbers.  
     * Will only rotate if nessescary.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number/object} xRot_radians__or__rotationVector The angle to rotate the vector along the x axis, in radians. Or, a 3d vector representing the various rotation degrees.
     * @param {number} yRot_radians The angle to rotate the vector along the y axis, in radians.
     * @param {number} zRot_radians The angle to rotate the vector along the z axis, in radians.
     *
     * @returns Itself, for chaining.
     */
    this.rotate = function(xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
        let xRot_radians;
        
        // Gets rotation angles from vectors.
        if (xRot_radians__or__rotationVector.type === 'vector3d') {
            xRot_radians = xRot_radians__or__rotationVector.x;
            yRot_radians = xRot_radians__or__rotationVector.y;
            zRot_radians = xRot_radians__or__rotationVector.z;
            
        // Gets rotation angles from raw numbers.
        } else {
            if (typeof xRot_radians__or__rotationVector !== 'number')
                xRot_radians = 0;
            else 
                xRot_radians = xRot_radians__or__rotationVector;
        
            if (typeof yRot_radians !== 'number')
                xRot_radians = 0;
        
            if (typeof zRot_radians !== 'number')
                xRot_radians = 0;
        }
        
        let oldX = this.x;
        let oldY = this.y;
        let oldZ = this.z;
        
        if (xRot_radians !== 0) {
            this.y = oldY * Math.cos(xRot_radians) - oldZ * Math.sin(xRot_radians);
            this.z = oldY * Math.sin(xRot_radians) + oldZ * Math.cos(xRot_radians);
            
            oldY = this.y;
            oldZ = this.z;
        }
        
        if (yRot_radians !== 0) {
            this.x = oldX * Math.cos(yRot_radians) + oldZ * Math.sin(yRot_radians);
            this.z = oldZ * Math.cos(yRot_radians) - oldX * Math.sin(yRot_radians);
            
            oldX = this.x;
        }
        
        if (zRot_radians !== 0) {
            this.x = oldX * Math.cos(zRot_radians) - oldY * Math.sin(zRot_radians);
            this.y = oldX * Math.sin(zRot_radians) + oldY * Math.cos(zRot_radians);
        }
        
        return this;
    }
    
    /**
     * @function
     *
     * Rotates the vector along the x, y, and z axes.
     *
     * Will only rotate if nessescary.
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number/object} xRot_radians__or__rotationVector The angle to rotate the vector along the x axis, in radians. Or, a 3d vector representing the various rotation degrees.
     * @param {number} yRot_radians The angle to rotate the vector along the y axis, in radians.
     * @param {number} zRot_radians The angle to rotate the vector along the z axis, in radians.
     *
     * @returns Itself, for chaining.
     */
    this.rotateUnsafely = function(xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
        let xRot_radians = 0;
        
        // Gets rotation angles from vectors.
        if (xRot_radians__or__rotationVector.type === 'vector3d') {
            xRot_radians = xRot_radians__or__rotationVector.x;
            yRot_radians = xRot_radians__or__rotationVector.y;
            zRot_radians = xRot_radians__or__rotationVector.z;
            
        } else
            xRot_radians = xRot_radians__or__rotationVector;
        
        let oldX = this.x;
        let oldY = this.y;
        let oldZ = this.z;
        
        if (xRot_radians !== 0) {
            this.y = oldY * Math.cos(xRot_radians) - oldZ * Math.sin(xRot_radians);
            this.z = oldY * Math.sin(xRot_radians) + oldZ * Math.cos(xRot_radians);
            
            oldY = this.y;
            oldZ = this.z;
        }
        
        if (yRot_radians !== 0) {
            this.x = oldX * Math.cos(yRot_radians) + oldZ * Math.sin(yRot_radians);
            this.z = oldZ * Math.cos(yRot_radians) - oldX * Math.sin(yRot_radians);
            
            oldX = this.x;
            oldZ = this.z;
        }
        
        if (zRot_radians !== 0) {
            this.x = oldX * Math.cos(zRot_radians) - oldY * Math.sin(zRot_radians);
            this.y = oldX * Math.sin(zRot_radians) + oldY * Math.cos(zRot_radians);
        }
        
        return this;
    }
    
    /**
     * @function
     *
     * Returns the vector as a string.
     *
     * @returns {string} The vector as a string.
     */
    this.toString = function() {
        return "[" + this.x + ", " + this.y + ", " + this.z + "]";
    }
}

/**
 * Translates the vector by (dx, dy, dz).
     *
 * Fails if vector is not a vector.
 * deltaX, deltaY, and deltaZ will be set to 0 if they are not numbers.
 *
 * @param {object} vector The vector to translate.
 * @param {number} deltaX How much to change the x component of the vector by.
 * @param {number} deltaY How much to change the y component of the vector by.
 * @param {number} deltaZ How much to change the z component of the vector by.
 *
 * @returns {object} A new vector that is the translation of vector.
 */
export function translate(vector, deltaX, deltaY, deltaZ) {
    if (vector.type !== 'vector3d')
        return;
    
    if (typeof deltaX !== 'number')
        deltaX = 0;

    if (typeof deltaY !== 'number')
        deltaY = 0;
    
    if (typeof deltaZ !== 'number')
        deltaZ = 0;
        
    let newX = vector.x + deltaX;
    let newY = vector.y + deltaY;
    let newZ  = vector.z + deltaZ;
        
    return new Vector3d(newX, newY, newZ);
}
    
/**
 * Translates the vector by (dx, dy, dz).
     *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to translate.
 * @param {number} deltaX How much to change the x component of the vector by.
 * @param {number} deltaY How much to change the y component of the vector by.
 * @param {number} deltaZ How much to change the z component of the vector by.
 *
 * @returns {object} A new vector that is the translation of vector.
 */
export function translateUnsafely(vector, deltaX, deltaY, deltaZ) {
    let newX = vector.x + deltaX;
    let newY = vector.y + deltaY;
    let newZ  = vector.z + deltaZ;
        
    return new Vector3d(newX, newY, newZ);
}
    
/**
 * Adds together two vectors.
 *
 * Fails if vector or otherVector are not vectors.
 *
 * @param {object} vector The first vector to be summed.
 * @param {object} otherVector The other vector to be summed.
 *
 * @returns {object} A new vector that is the sum of vector and otherVector.
 */
export function add(vector, otherVector) {
    if (vector.type !== 'vector3d')
        return;
    
    if (otherVector.type !== 'vector3d')
        return;
        
    let newX = vector.x + otherVector.x;
    let newY = vector.y + otherVector.y;
    let newZ = vector.z + otherVector.z;
        
    return new Vector3d(newX, newY, newZ);
}
   
/**
 * Adds together two vectors.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The first vector to be summed.
 * @param {object} otherVector The other vector to be summed.
 *
 * @returns {object} A new vector that is the sum of vector and otherVector.
 */
export function addUnsafely(vector, otherVector) {
    let newX = vector.x + otherVector.x;
    let newY = vector.y + otherVector.y;
    let newZ = vector.z + otherVector.z;
        
    return new Vector3d(newX, newY, newZ);
}
    
/**
 * Subtracts two vectors from eachother.
 *
 * Fails if vector or otherVector is not a vector.
 *
 * @param {object} vector The other vector, acting as the minuend.
 * @param {object} otherVector The other vector, acting as the subtrahend.
 *
 * @returns {object} A new vector that is the difference of vector and otherVector.
 */
export function subtract(vector, otherVector) {
    if (vector.type !== 'vector3d')
        return;
    
    if (otherVector.type !== 'vector3d')
        return;
        
    let newX = vector.x - otherVector.x;
    let newY = vector.y - otherVector.y;
    let newZ = vector.z - otherVector.z;
        
    return new Vector3d(newX, newY, newZ);
}
    
/**
 * Subtracts two vectors from eachother.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The other vector, acting as the minuend.
 * @param {object} otherVector The other vector, acting as the subtrahend.
 *
 * @returns {object} A new vector that is the difference of vector and otherVector.
 */
export function subtractUnsafely(vector, otherVector) {
    let newX = vector.x - otherVector.x;
    let newY = vector.y - otherVector.y;
    let newZ = vector.z - otherVector.z;
        
    return new Vector3d(newX, newY, newZ);
}
    
/**
 * Scales the vector by the scalar.
 *
 * Will fail if vector is not a vector.
 * Will return a null vector if scalar is not a number.
 *
 * @param {object} vector The vector to be scaled.
 * @param {number} scalar The amount to scale the vector by.
 *
 * @returns {object} A new vector that is the scaled version of vector.
 */
export function scale(vector, scalar) {
    if (typeof scalar !== 'number')
        return new Vector3d(0, 0, 0);
    
    if (vector.type !== 'vector3d')
        return;
        
    let newX = vector.x * scalar;
    let newY = vector.y * scalar;
    let newZ = vector.z * scalar;
        
    return new Vector3d(newX, newY, newZ);
}
    
/**
 * Scales the vector by the scalar.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to be scaled.
 * @param {number} scalar The amount to scale the vector by.
 *
 * @returns {object} A new vector that is the scaled version of vector.
 */
export function scaleUnsafely(vector, scalar) {
    let newX = vector.x * scalar;
    let newY = vector.y * scalar;
    let newZ = vector.z * scalar;
        
    return new Vector3d(newX, newY, newZ);
}

/**
 * Normalizes the vector, making its length 1.
 *
 * Will fail if vector is not a vector.
 * Returns a null vector if the length of vector is 0.
 *
 * @param {object} vector The vector to normalize.
 *
 * @returns {object} A new vector that is the normalized form of vector.
 */
export function normalize(vector) {
    if (vector.type !== 'vector3d')
        return;
    
    let length = vector.getLength();
        
    if (length != 0) {
        let newX = vector.x / length;
        let newY = vector.y / length;
        let newZ = vector.z / length;
        
        return new Vector3d(newX, newY, newZ);
        
    } else
        return new Vector3d(0, 0, 0);
}

/**
 * Normalizes the vector, making its length 1.
 *
 * Returns a null vector if the length of vector is 0.
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to normalize.
 *
 * @returns {object} A new vector that is the normalized form of vector.
 */
export function normalizeUnsafely(vector) {
    let length = vector.getLength();
        
    if (length != 0) {
        let newX = vector.x / length;
        let newY = vector.y / length;
        let newZ = vector.z / length;
        
        return new Vector3d(newX, newY, newZ);
        
    } else
        return new Vector3d(0, 0, 0);
}

/**
 * Rotates the vector along the x, y, and z axes.
 *
 * Fails if vector is not a vector.
 * Uses the first argument as the rotation angles if it is a 3d vector.
 * xRot, yRot, and zRot will be set to 0 if they are not numbers.  
 *
 * @param {object} vector The vector to rotate.
 * @param {number/object} xRot_radians__or__rotationVector The angle to rotate the vector along the x axis, in radians. Or, a 3d vector representing the various rotation degrees.
 * @param {number} yRot_radians The angle to rotate the vector along the y axis, in radians.
 * @param {number} zRot_radians The angle to rotate the vector along the z axis, in radians.
 *
 * @returns A new vector that is the rotation of vector.
 */
export function rotate(vector, xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
    if (vector.type !== 'vector3d')
        return;
    
    let xRot_radians;
        
    // Gets rotation angles from vectors.
    if (xRot_radians__or__rotationVector.type === 'vector3d') {
        xRot_radians = xRot_radians__or__rotationVector.x;
        yRot_radians = xRot_radians__or__rotationVector.y;
        zRot_radians = xRot_radians__or__rotationVector.z;
            
    // Gets rotation angles from raw numbers.
    } else {
        if (typeof xRot_radians__or__rotationVector !== 'number')                
            xRot_radians = 0;
        else
            xRot_radians = xRot_radians__or__rotationVector;
    
        if (typeof yRot_radians !== 'number')
            xRot_radians = 0;
        
        if (typeof zRot_radians !== 'number')
            xRot_radians = 0;
    }
    
    let newX = vector.x;
    let newY = vector.y;
    let newZ = vector.z;
    
    if (xRot_radians !== 0) {
        newY = newY * Math.cos(xRot_radians) - newZ * Math.sin(xRot_radians);
        newZ = newY * Math.sin(xRot_radians) + newZ * Math.cos(xRot_radians);
    }
        
    if (yRot_radians !== 0) {
        newX = newX * Math.cos(yRot_radians) + newZ * Math.sin(yRot_radians);
        newZ = newZ * Math.cos(yRot_radians) - newX * Math.sin(yRot_radians);
    }
        
    if (zRot_radians !== 0) {
        newX = newX * Math.cos(zRot_radians) - newY * Math.sin(zRot_radians);
        newY = newX * Math.sin(zRot_radians) + newY * Math.cos(zRot_radians);
    }
    
    return new Vector3d(newX, newY, newZ);
}

/**
 * Rotates the vector along the x, y, and z axes.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to rotate.
 * @param {number/object} xRot_radians__or__rotationVector The angle to rotate the vector along the x axis, in radians. Or, a 3d vector representing the various rotation degrees.
 * @param {number} yRot_radians The angle to rotate the vector along the y axis, in radians.
 * @param {number} zRot_radians The angle to rotate the vector along the z axis, in radians.
 *
 * @returns A new vector that is the rotation of vector.
 */
export function rotateUnsafely(vector, xRot_radians__or__rotationVector, yRot_radians, zRot_radians) {
    let xRot_radians;
        
    // Gets rotation angles from vectors.
    if (xRot_radians__or__rotationVector.type === 'vector3d') {
        xRot_radians = xRot_radians__or__rotationVector.x;
        yRot_radians = xRot_radians__or__rotationVector.y;
        zRot_radians = xRot_radians__or__rotationVector.z;
            
    // Gets rotation angles from raw numbers.
    } else
        xRot_radians = xRot_radians__or__rotationVector;
    
    let newX = vector.x;
    let newY = vector.y;
    let newZ = vector.z;
    
    if (xRot_radians !== 0) {
        newY = newY * Math.cos(xRot_radians) - newZ * Math.sin(xRot_radians);
        newZ = newY * Math.sin(xRot_radians) + newZ * Math.cos(xRot_radians);
    }
        
    if (yRot_radians !== 0) {
        newX = newX * Math.cos(yRot_radians) + newZ * Math.sin(yRot_radians);
        newZ = newZ * Math.cos(yRot_radians) - newX * Math.sin(yRot_radians);
    }
        
    if (zRot_radians !== 0) {
        newX = newX * Math.cos(zRot_radians) - newY * Math.sin(zRot_radians);
        newY = newX * Math.sin(zRot_radians) + newY * Math.cos(zRot_radians);
    }
    
    return new Vector3d(newX, newY, newZ);
}
