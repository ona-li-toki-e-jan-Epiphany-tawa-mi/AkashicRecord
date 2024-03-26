/*
 * MIT License
 *
 * Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * This is the JavaScript implementation of multiply-by-n, using an HTML canvas
 * and the canvas API to generate graphics.
 */



////////////////////////////////////////////////////////////////////////////////
// Config START                                                               //
////////////////////////////////////////////////////////////////////////////////
/**
 * The number of points on the circle.
 * @type {number}
 */
const points = 100

/**
 * A scalar applied to the size of the circle.
 * @type {number}
 */
const circleSizeMultiplier = 0.95

/**
 * A scalar applied to the speed of the progression of the animation.
 * @type {number}
 */
const animationSpeedMultiplier = 0.00015

/**
 * A scalar applied to the speed of the progression of the animation's
 * brightness.
 * @type {number}
 */
const brightnessSpeedMultipler = 0.01
////////////////////////////////////////////////////////////////////////////////
// Config END                                                                 //
////////////////////////////////////////////////////////////////////////////////



/**
 * HTML canvas with automatic resizing to their client size.
 *
 * @typedef {Object} SuperCanvas
 * @property {HTMLCanvasElement} element - The underlying canvas HTML element.
 * @property {CanvasRenderingContext2D} context - The canvas 2d context.
 */
class SuperCanvas {
    /**
     * Automates the resizing of super canvases to their client size.
     */
    static #superCanvasResizeObserver = new ResizeObserver((entries) => {
        for (const entry of entries) {
            const canvas = entry.target
            canvas.width  = canvas.clientWidth;
            canvas.height = canvas.clientHeight;
        }
    });

    /**
     * @param {HTMLCanvasElement} canvas - The canvas HTML element.
     * @param {boolean} [alpha=false] - Whether to enable alpha on the canvas.
     *                                  Defaults to false for optimization.
     */
    constructor(canvas, alpha=false) {
        this.element = canvas
        this.context = canvas.getContext("2d", {alpha: alpha})

        SuperCanvas.#superCanvasResizeObserver.observe(canvas)
    }

    /**
     * @type {number}
     */
    get width() {
        return this.element.width
    }

    /**
     * @type {number}
     */
    get height() {
        return this.element.height
    }
}



/**
 * Converts a point represented as a distance along the circumference of a
 * circle to an (x,y) position relative to that circle.
 *
 * @param {number} circumference - The cicumference of the circle.
 * @param {number} circumferenceDistance - The circumference distance of the
 *                                         point.
 * @param {number} radius - The radius of the circle.
 * @return {{x: number, y: number}} The resulting (x,y) position.
 */
function circumferenceToCoordinates(circumference, circumferenceDistance, radius) {
    const equivalentAngle = (2 * Math.PI / circumference) * circumferenceDistance
    return { x: Math.cos(equivalentAngle) * radius
           , y: Math.sin(equivalentAngle) * radius
           }
}



const canvas = new SuperCanvas(document.getElementById("canvas"))

// Line multipler - changes over time.
let multiplier = 0;
// Brightness of the animation.
let brightness = 0;
let deltaBrightness = brightnessSpeedMultipler;

let lastRenderTime = performance.now()
/**
 * Rendering loop for the canvas.
 *
 * Initialize using window.requestAnimationFrame().
 *
 * @param {DOMHighResTimeStamp} timeStamp - The timestamp from when the render
 *                              function was called.
 */
function render(timeStamp) {
    const deltaTime = timeStamp - lastRenderTime;
    lastRenderTime = timeStamp

    const {width, height, context} = canvas
    const circleRadius = Math.min(width, height) / 2 * circleSizeMultiplier

    multiplier += deltaTime * animationSpeedMultiplier
    brightness += deltaBrightness * deltaTime
    if (brightness < 0 || brightness > 255) {
        brightness = Math.max(0, Math.min(255, brightness))
        deltaBrightness *= -1
    }

    // Clears the screen
    context.clearRect(0, 0, width, height)


    // Moves the animation to the center of the screen.
    context.save()
    context.translate(width / 2, height / 2)
    const flooredBrightness = Math.floor(brightness)
    context.strokeStyle = `rgb(${flooredBrightness}, ${flooredBrightness}, ${flooredBrightness})`

    // Draws the lines between the points and their multiplied equivalents on
    // the circle.
    context.beginPath()
    for (let point = 0; point < points; point++) {
        const otherPoint = (point * multiplier) % points
        const {x: startX, y: startY} = circumferenceToCoordinates(points, point, circleRadius)
        const {x: endX, y: endY}     = circumferenceToCoordinates(points, otherPoint, circleRadius)

        // Y values are flipped in the left-handed coordinate space of the
        // canvas.
        context.moveTo(startX, -startY)
        context.lineTo(endX, -endY)
    }
    context.stroke()

    // Draws the border of the circle.
    context.beginPath()
    context.arc(0, 0, circleRadius, 0, Math.PI * 2)
    context.stroke()

    // Restore from translation.
    context.restore()


    window.requestAnimationFrame(render)
}
window.requestAnimationFrame(render)
