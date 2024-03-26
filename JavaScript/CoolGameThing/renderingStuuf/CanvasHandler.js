/**
 * @class
 *
 * Handles interactions between a canvas and external code.
 */
export function CanvasHandler(canvas) {
    this.type = 'canvashandler';
    
    this.canvas = canvas;
    this.canvasContext = this.canvas.getContext('2d');
    this.width = this.canvasContext.width;
    this.height = this.canvasContext.height;
    this.halfWidth = this.width / 2;
    this.halfHeight = this.height / 2;
    
    // Stores color information for the pixels.
    this.colorMatrix = [];
    
    /**
     * @function
     *
     * Binds the CanvasHandler to a new canvas.
     *
     * Throws errors if newCanvas is not a canvas.
     * Faulty input will not damage the CanvasHandler.
     *
     * @param {object} newCanvas The new canvas that is to be bound to the CanvasHandler.
     */
    this.setCanvas = function(newCanvas) {
        // Protects the CanvasHandler instance from faulty code by acting as a break point.
        newCanvas.getContext('2d');
        
        this.canvas = newCanvas;
        this.canvasContext = this.canvas.getContext('2d');
        this.width = this.canvasContext.width;
        this.height = this.canvasContext.height;
    }
    
    /**
     * @function
     *
     * Binds the CanvasHandler to a new canvas.
     *
     * WARNING!: does not account for faulty input of any kind.
     * Faulty input will damage the CanvasHandler.
     *
     * @param {object} newCanvas The new canvas that is to be bound to the CanvasHandler.
     */
    this.setCanvasUnsafely = function(newCanvas) {    
        this.canvas = newCanvas;
        this.canvasContext = this.canvas.getContext('2d');
        this.width = this.canvasContext.width;
        this.height = this.canvasContext.height;
    }
    
    /**
     * @function
     *
     * Resets the canvas' size to the size of the window.
     * Resizes the color matrix to the size of the canvas.
     *
     * Fails if width or height are not numbers.
     *
     * @param {number} width The width to set the canvas to, if need be.
     * @param {number} height The height to set the canvas to, if need be.
     */
     this.refreshCanvas = function(width, height) {
        if (typeof width !== 'number' || typeof height !== 'number')
            return;
    
        if (this.canvas.width != width) {
            this.canvas.width = width;
            this.width = this.canvas.width;
            this.halfWidth = this.width / 2;
        }
    
        if (this.canvas.height != height) {
            this.canvas.height = height;
            this.height = this.canvas.height;
            this.halfHeight = this.height / 2;
        }
    
        setColorMatrixSize(this.width, this.height, this.colorMatrix); 
    }
     
    /**
     * @function
     *
     * Resets the canvas' size to the size of the window.
     * Resizes the color matrix to the size of the canvas.
     *
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {number} width The width to set the canvas to, if need be.
     * @param {number} height The height to set the canvas to, if need be.
     */
     this.refreshCanvasUnsafely = function(width, height) {
        if (this.canvas.width != width) {
            this.canvas.width = width;
            this.width = this.canvas.width;
            this.halfWidth = this.width / 2;
        }
    
        if (this.canvas.height != height) {
            this.canvas.height = height;
            this.height = this.canvas.height;    
            this.halfHeight = this.height / 2;
        }
    
        setColorMatrixSize(this.width, this.height, this.colorMatrix); 
    }
    
    /**
     * @function
     *
     * Sets a pixel at (x, y) in the canvas to be rgba(red, green. blue, alpha).
     *
     * Will not run if x or y are not numbers.
     * Will not run if x or y are outside canvas boundaries.
     * Rounds x and y down to the nearest integer.
     * Constrains red, green, blue, and alpha to be between 0 and 255.
     * Sets red, green, and blue to 0 if they are not numbers.
     * Sets alpha to 255 if it's not a number.
     *
     * @param {number} x The x position of the pixel to be set. Must be: 0 <= x < width. 
     * @param {number} y The y position of the pixel to be set. Must be: 0 <= y < height.
     * @param {number} red The value of the pixel's red channel to be set. Gets constrained to: 0 <= red <= 255.
     * @param {number} green The value of the pixel's green channel to be set. Gets constrained to: 0 <= green <= 255.
     * @param {number} blue The value of the pixel's blue channel to be set. Gets constrained to: 0 <= blue <= 255.
     * @param {number} alpha The value of the pixel's alpha channel to be set. Gets constrained to: 0 <= alpha <= 255.
     */
    this.setPixel = function(x, y, red, green, blue, alpha) {
        if (typeof x !== 'number' || typeof y !== 'number')
            return;
        
        if (x < 0 || x >= this.width || y < 0 || y >= this.height)
            return;
    
        // Makes sure x and y are integer amounts.
        x = Math.floor(x);
        y = Math.floor(y);
    
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
    
        this.colorMatrix[y][x].red = red;
        this.colorMatrix[y][x].green = green;
        this.colorMatrix[y][x].blue = blue;
        this.colorMatrix[y][x].alpha = alpha;
    }
    
    /**
     * @function
     *
     * Sets a pixel at (x, y) in the canvas to be rgba(red, green. blue, alpha).
     *
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {number} x The x position of the pixel to be set. Should be: 0 <= x < width. 
     * @param {number} y The y position of the pixel to be set. Should be: 0 <= y < height.
     * @param {number} red The value of the pixel's red channel to be set. Should be: 0 <= red <= 255.
     * @param {number} green The value of the pixel's green channel to be set. Should be: 0 <= green <= 255.
     * @param {number} blue The value of the pixel's blue channel to be set. Should be: 0 <= blue <= 255.
     * @param {number} alpha The value of the pixel's alpha channel to be set. Should be: 0 <= alpha <= 255.
     */
    this.setPixelUnsafely = function(x, y, red, green, blue, alpha) {
        this.colorMatrix[y][x].red = red;
        this.colorMatrix[y][x].green = green;
        this.colorMatrix[y][x].blue = blue;
        this.colorMatrix[y][x].alpha = alpha;
    }
   
    /**
     * @function
     *
     * Draws a line from (x1, y1) to (x2, y2) on the canvas.
     *
     * x1, y1, x2, and y2 will be set to 0 if they are not numbers.
     * Only draws pixels onto the canvas if they lie on it.
     * Constrains red, green, blue, and alpha to be between 0 and 255.
     * Sets red, green, and blue to 0 if they are not numbers.
     * Sets alpha to 255 if it's not a number.
     *
     * @param {number} x1 The x position of the first pixel of the line. 
     * @param {number} y1 The y position of the first pixel of the line.
     * @param {number} x2 The x position of the second pixel of the line. 
     * @param {number} y2 The y position of the second pixel of the line.
     * @param {number} red The value of the pixels' red channel to be set. Gets constrained to: 0 <= red <= 255.
     * @param {number} green The value of the pixels' green channel to be set. Gets constrained to: 0 <= green <= 255.
     * @param {number} blue The value of the pixels' blue channel to be set. Gets constrained to: 0 <= blue <= 255.
     * @param {number} alpha The value of the pixels' alpha channel to be set. Gets constrained to: 0 <= alpha <= 255.
     */
    this.drawLine = function(x1, y1, x2, y2, red, green, blue, alpha) {
        if (typeof x1 !== 'number')
            x1 = 0;
        
        if (typeof y1 !== 'number')
            y1 = 0;
        
        if (typeof x2 !== 'number')
            x2 = 0;
        
        if (typeof y2 !== 'number')
            y2 = 0;
        
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
        
        // m = 0.
        if (x1 === x2) {
            for (let y = Math.floor(Math.min(y1, y2)); y < Math.max(y1, y2); y++) {
                let x = Math.floor(x1);
                
                if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                    this.setPixelUnsafely(x, y, red, green, blue, alpha);
            }
        
        // m = infinity.
        } else if (y1 === y2) { 
            for (let x = Math.floor(Math.min(x1, x2)); x < Math.max(x1, x2); x++) {
                let y = Math.floor(y1);
                
                if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                    this.setPixelUnsafely(x, y, red, green, blue, alpha);
            }
        
        } else {
            let slope = (y1 - y2) / (x1 - x2);
            let intercept = y1 - slope * x1;
            
            // -1 <= m <= 1.
            if (slope >= - 1 && slope <= 1) {
                for (let x = Math.floor(Math.min(x1, x2)); x < Math.max(x1, x2); x++) {
                    let y = Math.floor(x * slope + intercept);
                    
                    if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                        this.setPixelUnsafely(x, y, red, green, blue, alpha);
                }
            
            // m < -1 or m > 1. 
            } else 
                for (let y = Math.floor(Math.min(y1, y2)); y < Math.max(y1, y2); y++) {
                    let x = Math.floor((y - intercept) / slope);
                    
                    if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                        this.setPixelUnsafely(x, y, red, green, blue, alpha);
                }
        }
    }

    /**
     * @function
     *
     * Draws a line from (x1, y1) to (x2, y2) on the canvas.
     *
     * Only draws pixels onto the canvas if they lie on it.
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {number} x1 The x position of the first pixel of the line. 
     * @param {number} y1 The y position of the first pixel of the line.
     * @param {number} x2 The x position of the second pixel of the line. 
     * @param {number} y2 The y position of the second pixel of the line.
     * @param {number} red The value of the pixels' red channel to be set. Should be: 0 <= red <= 255.
     * @param {number} green The value of the pixels' green channel to be set. Should be: 0 <= green <= 255.
     * @param {number} blue The value of the pixels' blue channel to be set. Should be: 0 <= blue <= 255.
     * @param {number} alpha The value of the pixels' alpha channel to be set. Should be: 0 <= alpha <= 255.
     */
    this.drawLineUnsafely = function(x1, y1, x2, y2, red, green, blue, alpha) {
        // m = 0.
        if (x1 === x2) {
            for (let y = Math.floor(Math.min(y1, y2)); y < Math.max(y1, y2); y++) {
                let x = Math.floor(x1);
                
                if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                    this.setPixelUnsafely(x, y, red, green, blue, alpha);
            }
        
        // m = infinity.
        } else if (y1 === y2) { 
            for (let x = Math.floor(Math.min(x1, x2)); x < Math.max(x1, x2); x++) {
                let y = Math.floor(y1);
                
                if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                    this.setPixelUnsafely(x, y, red, green, blue, alpha);
            }
        
        } else {
            let slope = (y1 - y2) / (x1 - x2);
            let intercept = y1 - slope * x1;
            
            // -1 <= m <= 1.
            if (slope >= - 1 && slope <= 1) {
                for (let x = Math.floor(Math.min(x1, x2)); x < Math.max(x1, x2); x++) {
                    let y = Math.floor(x * slope + intercept);
                    
                    if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                        this.setPixelUnsafely(x, y, red, green, blue, alpha);
                }
            
            // m < -1 or m > 1. 
            } else 
                for (let y = Math.floor(Math.min(y1, y2)); y < Math.max(y1, y2); y++) {
                    let x = Math.floor((y - intercept) / slope);
                    
                    if (x >= 0 && x < this.width && y >= 0 && y < this.height)
                        this.setPixelUnsafely(x, y, red, green, blue, alpha);
                }
        }
    }
     
    /**
     * Gets the color data from a pixel on the canvas.
     *
     * Will fail if x or y are not numbers.
     *
     * @param {number} x The x postion of the pixel to get the data from.
     * @param {number} y The y postion of the pixel to get the data from.
     *
     * @returns {object} An object containing the value of each color channel in the pixel.
     */
    this.getPixel = function(x, y) {
        if (typeof x !== 'number' || typeof y !== 'number')
            return;
    
        return {
            red: this.colorMatrix[y][x].red,
            green: this.colorMatrix[y][x].green,
            blue: this.colorMatrix[y][x].blue,
            alpha: this.colorMatrix[y][x].alpha
        };
    }
    
    /**
     * Gets the color data from a pixel on the canvas.
     *
     * WARNING!: does not account for faulty input of any kind.
     *
     * @param {number} x The x postion of the pixel to get the data from.
     * @param {number} y The y postion of the pixel to get the data from.
     *
     * @returns {object} An object containing the value of each color channel in the pixel.
     */
    this.getPixelUnsafely = function(x, y) {
        return {
            red: this.colorMatrix[y][x].red,
            green: this.colorMatrix[y][x].green,
            blue: this.colorMatrix[y][x].blue,
            alpha: this.colorMatrix[y][x].alpha
        };
    }
    
    /**
     * Sets all pixels on the canvas to white.
     */
    this.clearCanvas = function() {
        for (let y = 0; y < this.colorMatrix.length; y++)
            for (let x = 0; x < this.colorMatrix[y].length; x++) {
                this.colorMatrix[y][x].red = 0;
                this.colorMatrix[y][x].green = 0;
                this.colorMatrix[y][x].blue = 0;
                this.colorMatrix[y][x].alpha = 0;
            }
    }
    
    /**
     * Draws the pixels that were set onto the canvas.
     */
    this.updateCanvas = function() {
        let canvasData = this.canvasContext.getImageData(0, 0, this.width, this.height);
        let canvasDataData = canvasData.data;
    
        for (let y = 0; y < this.colorMatrix.length; y++)
            for (let x = 0; x < this.colorMatrix[y].length; x++) {
                let pixelIndex = getPixelIndex(x, y, this.width);
            
                canvasDataData[pixelIndex] = this.colorMatrix[y][x].red;
                canvasDataData[pixelIndex + 1] = this.colorMatrix[y][x].green;
                canvasDataData[pixelIndex + 2] = this.colorMatrix[y][x].blue;
                canvasDataData[pixelIndex + 3] = this.colorMatrix[y][x].alpha;
            }
    
        this.canvasContext.putImageData(canvasData, 0, 0);
    }
    
    /**
     * @function
     *
     * Returns the CanvasHandler as a string.
     *
     * @returns {string} The CanvasHandler as a string.
     */
    this.toString = function() {
        return "{type: " + this.type + ", canvas: " + this.canvas.toString() + ", width: " + this.width + ", height: " + this.height + "}";
    }
    
    this.refreshCanvasUnsafely(this.width, this.height);
}

/**
 * Changes the size of the color matrix to h rows and w columns.
 *
 * Only changes the matrix size if need be.
 * For internal use only.
 *
 * @param {number} width The new width of the color matrix. Should be: w > 0.
 * @param {number} height The new height of the color matrix. Should be: h > 0.
 * @param {object} colorMatrix The new height of the color matrix. 
 */
function setColorMatrixSize(width, height, colorMatrix) {
    // Adjusts row length.
    for (let y = 0; y < colorMatrix.length; y++)
        if (width < colorMatrix[y].length) {
            colorMatrix[y].splice(width, colorMatrix[y].length - width);
            
        } else if (width > colorMatrix[y].length)
            for (let x = 0; x < width - colorMatrix[y].length; x++) {
                colorMatrix[y].push({});
                
                colorMatrix[y][colorMatrix[y].length - 1].red = 0;
                colorMatrix[y][colorMatrix[y].length - 1].green = 0;
                colorMatrix[y][colorMatrix[y].length - 1].blue = 0;
                colorMatrix[y][colorMatrix[y].length - 1].alpha = 0;
            }
    
    // Adjusts column length.
    if (height < colorMatrix.length) {
        colorMatrix.splice(height, colorMatrix.length - height);
    
    } else if (height > colorMatrix.length)
        for (let y = 0; y < height - colorMatrix.length; y++) {
            colorMatrix.push([]);
            
            for (let x = 0; x < width; x++) {
                colorMatrix[colorMatrix.length - 1][x] = {};
                
                colorMatrix[colorMatrix.length - 1][x].red = 0;
                colorMatrix[colorMatrix.length - 1][x].green = 0;
                colorMatrix[colorMatrix.length - 1][x].blue = 0;
                colorMatrix[colorMatrix.length - 1][x].alpha = 0;
            }
        }
}

/**
 * Gets the index of a pixel in the data array that canvas#ImageData has.
 *
 * For internal use only.
 *
 * @param {number} x The x position of the pixel.
 * @param {number} y The y position of the pixel.
 */
function getPixelIndex(x, y, width) {
    return ((y * width) << 2) + (x << 2);
}
