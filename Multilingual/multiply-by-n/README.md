# multiply-by-n

This project is an idea I had to write this cool animation in every single programming language I learn that I can figure out a way to do it in.

The animation consists of the following steps:

- Generate a set of evenly-spaced points along a circle, represented using the distance along the circumference from the top of the circle.
- Take the circumference-distance value of each point and multiply it by some multiplier that is increased as the animation progresses, applying a rolling clamp to make sure the distance value of the new point is within the circumference of the circle.
- The original point and it's multiplied partner forms a chord across the circle. The circumference distance values are then converted to the corresponding (x,y) positions along the unit circle and scaled to the desired size of the image.

I also have it cycle the opacity/brightness.

You can find each version of multiply-by-n in the subdirectories, labled with the language and graphics library (or equivalent system) used, along with instructions on how to run it. There's also a live demo for the JavaScript-Canvas implementation.
