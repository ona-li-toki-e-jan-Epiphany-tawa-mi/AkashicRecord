/**
 * @class
 *
 * A 2 dimensional vector, loaded with vector math.
 *
 * @param {number} x The x component of the vector to be created.
 * @param {number} y The y component of the vector to be created.
 */
export function Vector2d(x, y) {
    this.type = 'vector2d';
    
    if (typeof x !== 'number')
        x = 0;
    
    if (typeof y !== 'number')
        y = 0;
    
    this.x = x;
    this.y = y;
    
    /**
     * @function
     *
     * Translates the vector by (dx, dy).
     *
     * deltaX, deltaY will be set to 0 if they are not numbers.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} deltaX How much to change the x component of the vector by.
     * @param {number} deltaY How much to change the y component of the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.translate = function(deltaX, deltaY) {
        if (typeof deltaX !== 'number')
            deltaX = 0;
    
        if (typeof deltaY !== 'number')
            deltaY = 0;
        
        this.x += deltaX;
        this.y += deltaY;
        
        return this;
    }
    
    /**
     * @function
     *
     * Translates the vector by (dx, dy).
     *
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} deltaX How much to change the x component of the vector by.
     * @param {number} deltaY How much to change the y component of the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.translateUnsafely = function(deltaX, deltaY) {
        this.x += deltaX;
        this.y += deltaY;
        
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
        if (otherVector.type !== 'vector2d')
            return;
        
        this.x += otherVector.x;
        this.y += otherVector.y;
        
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
        if (otherVector.type !== 'vector2d')
            return;
        
        this.x -= otherVector.x;
        this.y -= otherVector.y;
        
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
        
        return this;
    }
    
    /**
     * @function
     *
     * Scales the vector by the scalar.
     *
     * Will return a null vector if scalar is not a number.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} scalar The amount to scale the vector by.
     *
     * @returns {object} Itself, for chaining.
     */
    this.scale = function(scalar) {
        if (typeof scalar !== 'number')
            scalar = 0;
        
        this.x *= scalar;
        this.y *= scalar;
        
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
        return this.x * this.x + this.y * this.y;
    }
    
    /**
     * @function
     *
     * Gets the length of the vector.
     *
     * @returns {number} The length of the vector.
     */
    this.getLength = function() {
        return Math.sqrt(this.x * this.x + this.y * this.y);
    }
    
    /**
     * @function
     *
     * Normalizes the vector, making its length 1.
     *
     * Does nothings if the length of the vector is 0.
     * WARNING!: this function will mutate the vector.
     *
     * @returns {object} Itself, for chaining.
     */
    this.normalize = function() {
        let length = this.getLength();
        
        if (length != 0) {
            this.x /= length;
            this.y /= length;
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
     * @returns {number} The dot product.
     */
    this.getDotProduct = function(otherVector) {
        if (otherVector.type !== 'vector2d')
            return;
        
        return this.x * otherVector.x + this.y * otherVector.y;
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
     * @returns {number} The dot product.
     */
    this.getDotProductUnsafely = function(otherVector) {
        return this.x * otherVector.x + this.y * otherVector.y;
    }
    
    /**
     * @function
     *
     * Gets the cross product of two vectors.
     *
     * Will fail if otherVector is not a vector.
     *
     * @param {object} otherVector The other vector to be used to find the cross product.
     *
     * @returns {number} The cross product of the two vectors.
     */
    this.getCrossProduct = function(otherVector) {
        if (otherVector.type !== 'vector2d')
            return;
        
        return this.x * otherVector.y - otherVector.x * this.y;
    }
    
    /**
     * @function
     *
     * Gets the cross product of two vectors.
     *
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {object} otherVector The other vector to be used to find the cross product.
     *
     * @returns {number} The cross product of the two vectors.
     */
    this.getCrossProductUnsafely = function(otherVector) {
        return this.x * otherVector.y - otherVector.x * this.y;
    }
    
    /**
     * @function
     *
     * Rotates a vector by theta_radians radians.
     * Positive angles are counter-clockwise, and negative angles are clockwise.
     *
     * Does nothing (basically rotating by 0 radians) if theta_radians is not a number.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} theta_radians The angle to rotate the vector by, in radians.
     *
     * @returns {object} Itself, for chaining.
     */
    this.rotate = function(theta_radians) {
        if (typeof theta_radians !== 'number')
            return this;
        
        let newX = Math.cos(theta_radians) * this.x - Math.sin(theta_radians) * this.y;
        let newY = Math.sin(theta_radians) * this.x + Math.cos(theta_radians) * this.y;
        
        this.x = newX;
        this.y = newY;
        
        return this;
    }
    
    /**
     * @function
     *
     * Rotates a vector by theta_radians radians.
     * Positive angles are counter-clockwise, and negative angles are clockwise.
     *
     * WARNING!: does not account for faulty input of any kind.
     * WARNING!: this function will mutate the vector.
     *
     * @param {number} theta_radians The angle to rotate the vector by, in radians.
     *
     * @returns {object} Itself, for chaining.
     */
    this.rotateUnsafely = function(theta_radians) {
        let newX = Math.cos(theta_radians) * this.x - Math.sin(theta_radians) * this.y;
        let newY = Math.sin(theta_radians) * this.x + Math.cos(theta_radians) * this.y;
        
        this.x = newX;
        this.y = newY;
        
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
        return "[" + this.x + ", " + this.y + "]";
    }
}

/**
 * Translates the vector by (dx, dy).
 *
 * Fails if vector is not a vector.
 * deltaX, deltaY will be set to 0 if they are not numbers.
 *
 * @param {object} vector The vector to translate.
 * @param {number} deltaX How much to change the x component of the vector by.
 * @param {number} deltaY How much to change the y component of the vector by.
 *
 * @returns {object} A new vector that is the translation of the original.
 */
export function translate(vector, deltaX, deltaY) {
    if (vector.type !== 'vector2d')
        return;
    
    if (typeof deltaX !== 'number')
        deltaX = 0;
    
    if (typeof deltaY !== 'number')
        deltaY = 0;
        
    let newX = vector.x + deltaX;
    let newY = vector.y + deltaY;
    
    return new Vector2d(newX, newY);
}
    
/**
 * Translates the vector by (dx, dy).
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to translate.
 * @param {number} deltaX How much to change the x component of the vector by.
 * @param {number} deltaY How much to change the y component of the vector by.
 *
 * @returns {object} A new vector that is the translation of the original.
 */
export function translateUnsafely(vector, deltaX, deltaY) {
    let newX = vector.x + deltaX;
    let newY = vector.y + deltaY;
    
    return new Vector2d(newX, newY);
}
    
/**
 * Adds together two vectors.
 *
 * Fails if vector or otherVector are not vectors.
 *
 * @param {object} vector The vector to be summed.
 * @param {object} otherVector The other vector to be summed.
 *
 * @returns {object} A new vector that is the sum of vector and otherVector.
 */
export function add(vector, otherVector) {
    if (vector.type !== 'vector2d')
        return;
    
    if (otherVector.type !== 'vector2d')
        return;
        
    let newX = vector.x + otherVector.x;
    let newY = vector.y + otherVector.y;
        
    return new Vector2d(newX, newY);
}
    
/**
 * Adds together two vectors.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to be summed.
 * @param {object} otherVector The other vector to be summed.
 *
 * @returns {object} A new vector that is the sum of vector and otherVector.
 */
export function addUnsafely(vector, otherVector) {
    let newX = vector.x + otherVector.x;
    let newY = vector.y + otherVector.y;
        
    return new Vector2d(newX, newY);
}
    
/**
 * Subtracts two vectors from eachother.
 *
 * Fails if vector or otherVector are not vectors.
 *
 * @param {object} vector The vector, acting as the minuend.
 * @param {object} otherVector The other vector, acting as the subtrahend.
 *
 * @returns {object} A new vector that is the difference of vector and otherVector.
 */
export function subtract(vector, otherVector) {
    if (vector.type !== 'vector2d')
        return;
    
    if (otherVector.type !== 'vector2d')
        return;
        
    let newX = vector.x - otherVector.x;
    let newY = vector.y - otherVector.y;
        
    return new Vector2d(newX, newY);
}
    
/**
 * Subtracts two vectors from eachother.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector, acting as the minuend.
 * @param {object} otherVector The other vector, acting as the subtrahend.
 *
 * @returns {object} A new vector that is the difference of vector and otherVector.
 */
export function subtractUnsafely(vector, otherVector) {
    let newX = vector.x - otherVector.x;
    let newY = vector.y - otherVector.y;
        
    return new Vector2d(newX, newY);
}
    
/**
 * Scales the vector by the scalar.
 *
 * Will fail if vector is not a vector
 * Will return a null vector if scalar is not a number.
 *
 * @param {object} vector The vector to be scaled.
 * @param {number} scalar The amount to scale the vector by.
 *
 * @returns {object} A new vector that is a scaled version of vector.
 */
export function scale(vector, scalar) {
    if (typeof scalar !== 'number')
        return new Vector2d(0, 0);
    
    if (vector.type !== 'vector2d')
        return;
        
    let newX = vector.x * scalar;
    let newY = vector.y * scalar;
        
    return new Vector2d(newX, newY);
}
    
/**
 * Scales the vector by the scalar.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to be scaled.
 * @param {number} scalar The amount to scale the vector by.
 *
 * @returns {object} A new vector that is a scaled version of vector.
 */
export function scaleUnsafely(vector, scalar) {
    let newX = vector.x * scalar;
    let newY = vector.y * scalar;
        
    return new Vector2d(newX, newY);
}

/**
 * Normalizes the vector, making its length 1.
 *
 * Returns a null vector if the length of vector is 0.
 * Fails if vector is not a vector.
 *
 * @param {object} vector The vector to be normalized.
 *
 * @returns {object} A new vector that is the normalized form of vector. 
 */
export function normalize(vector) {
    if (vector.type !== 'vector2d')
        return;
    
    let length = vector.getLength();
        
    if (length != 0) {
        let newX = vector.x / length;
        let newY = vector.y / length;
        
        return new Vector2d(newX, newY);
        
    } else
        return new Vector2d(0, 0);
}

/**
 * Normalizes the vector, making its length 1.
 *
 * Returns a null vector if the length of vector is 0.
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to be normalized.
 *
 * @returns {object} A new vector that is the normalized form of vector. 
 */
export function normalizeUnsafely(vector) {
    let length = vector.getLength();
        
    if (length != 0) {
        let newX = vector.x / length;
        let newY = vector.y / length;
        
        return new Vector2d(newX, newY);
        
    } else
        return new Vector2d(0, 0);
}

/**
 * Rotates a vector by theta_radians radians.
 * Positive angles are counter-clockwise, and negative angles are clockwise.
 *
 * Fails if vector is not a vector.
 * Returns vector as it was (basically rotating by 0 radians) if theta_radians is not a number.
 *
 * @param {object} vector The vector to rotate.
 * @param {number} theta_radians The angle to rotate the vector by, in radians.
 *
 * @returns {object} A new vector that is the rotation of vector.
 */
export function rotate(vector, theta_radians) {
    if (vector.type !== 'vector2d')
        return;
    
    if (typeof theta_radians !== 'number')
        return new Vector2d(vector.x, vector,y);
    
    let newX = Math.cos(theta_radians) * this.x - Math.sin(theta_radians) * this.y;
    let newY = Math.sin(theta_radians) * this.x + Math.cos(theta_radians) * this.y;
        
    return new Vector2d(newX, newY);
}
    
/**
 * Rotates a vector by theta_radians radians.
 * Positive angles are counter-clockwise, and negative angles are clockwise.
 *
 * WARNING!: does not account for faulty input of any kind.
 *
 * @param {object} vector The vector to rotate.
 * @param {number} theta_radians The angle to rotate the vector by, in radians.
 *
 * @returns {object} A new vector that is the rotation of vector.
 */
export function rotateUnsafely(vector, theta_radians) {
    let newX = Math.cos(theta_radians) * this.x - Math.sin(theta_radians) * this.y;
    let newY = Math.sin(theta_radians) * this.x + Math.cos(theta_radians) * this.y;
        
    return new Vector2d(newX, newY);
}
    