package com.fortilituda.epiphany.maths;

/**
 * A 2d vector, filled with vector math methods.
 */
public class Vector2d {
    private double x, y;

    /**
     * Creates a new null 2d vector.
     */
    public Vector2d() {
        x = 0;
        y = 0;
    }

    /**
     * Creates a new 2d vector.
     *
     * @param x The x component of the vector.
     * @param y The y component of the vector.
     */
    public Vector2d(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Creates a copy of vector.
     *
     * @param vector The vector to copy.
     */
    public Vector2d(Vector2d vector) {
        x = vector.x;
        y = vector.y;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
    }

    /**
     * Translates the vector by (deltaX, deltaY.)
     *
     * Mutates the current vector.
     *
     * @param deltaX The amount to change the x component by.
     * @param deltaY The amount to change the y component by.
     *
     * @return Itself, for chaining.
     */
    public Vector2d translate(double deltaX, double deltaY) {
        x += deltaX;
        y += deltaY;

        return this;
    }

    /**
     * Adds otherVector to the current vector.
     *
     * Mutates the current vector.
     *
     * @param otherVector The vector to add with.
     *
     * @return Itself, for chaining.
     */
    public Vector2d add(Vector2d otherVector) {
        x += otherVector.x;
        y += otherVector.y;

        return this;
    }

    /**
     * Subtracts otherVector from the current vector.
     *
     * Mutates the current vector.
     *
     * @param otherVector The vector to subtract with.
     *
     * @return Itself, for chaining.
     */
    public Vector2d subtract(Vector2d otherVector) {
        x -= otherVector.x;
        y -= otherVector.y;

        return this;
    }

    /**
     * Scales the current vector by scalar.
     *
     * Mutates the current vector.
     *
     * @param scalar The amount to scale by.
     *
     * @return Itself, for chaining.
     */
    public Vector2d scale(double scalar) {
        x *= scalar;
        y *= scalar;

        return this;
    }

    /**
     * Gets the length of the current vector, but leaves it squared.
     *
     * @return The length of the current vector, squared.
     */
    public double getLengthSquared() {
        return x * x + y * y;
    }

    /**
     * Gets the length of the current vector.
     *
     * @return The length of the current vector.
     */
    public double getLength() {
        return Math.sqrt(x * x + y * y);
    }

    /**
     * Normalizes the vector, making its length 1.
     *
     * Does nothing if the length of the vector is 0.
     * Mutates the current vector.
     *
     * @return Itself, for chaining.
     */
    public Vector2d normalize() {
        double length = getLength();

        if (length != 0) {
            x /= length;
            y /= length;
        }

        return this;
    }

    /**
     * Gets the dot product of the current vector and otherVector.
     *
     * @param otherVector The other vector to use to find the dot product.
     *
     * @return The dot product of the current vector and otherVector.
     */
    public double getDotProduct(Vector2d otherVector) {
        return x * otherVector.x + y * otherVector.y;
    }

    /**
     * Gets the cross product of the current vector and otherVector.
     *
     * @param otherVector The other vector to use to find the cross product.
     *
     * @return The cross product of the current vector and otherVector.
     */
    public double getCrossProduct(Vector2d otherVector) {
        return x * otherVector.y - y * otherVector.x;
    }

    /**
     * Rotates the current vector by radians radians.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * Mutates the current vector.
     *
     * @param radians The amount to rotate the vector by, in radians.
     *
     * @return Itself, for chaining.
     */
    public Vector2d rotate(double radians) {
        double newX = x * Math.cos(radians) - y * Math.sin(radians);
        double newY = x * Math.sin(radians) + y * Math.cos(radians);

        x = newX;
        y = newY;

        return this;
    }

    @Override
    public String toString() {
        return "[" + x + ", " + y + "]";
    }

    /**
     * Translates the vector by (deltaX, deltaY.)
     *
     * @param vector The vector to translate to.
     * @param deltaX The amount to change the x component by.
     * @param deltaY The amount to change the y component by.
     *
     * @return A new vector that is the translation of vector.
     */
    public static Vector2d translate(Vector2d vector, double deltaX, double deltaY) {
        double newX = vector.x + deltaX;
        double newY = vector.y + deltaY;

        return new Vector2d(newX, newY);
    }

    /**
     * Adds otherVector to the vector.
     *
     * @param vector The vector to add to.
     * @param otherVector The vector to add with.
     *
     * @return A new vector that is the sum of vector and otherVector.
     */
    public static Vector2d add(Vector2d vector, Vector2d otherVector) {
        double newX = vector.x + otherVector.x;
        double newY = vector.y + otherVector.y;

        return new Vector2d(newX, newY);
    }

    /**
     * Subtracts otherVector from vector.
     *
     * @param vector The vector to subtract from.
     * @param otherVector The vector to subtract with.
     *
     * @return A new vector that is the difference of vector and otherVector.
     */
    public static Vector2d subtract(Vector2d vector, Vector2d otherVector) {
        double newX = vector.x - otherVector.x;
        double newY = vector.y - otherVector.y;

        return new Vector2d(newX, newY);
    }

    /**
     * Scales the current vector by scalar.
     *
     * @param vector The vector to scale.
     * @param scalar The amount to scale by.
     *
     * @return A new vector that is the scaled form of vector.
     */
    public static Vector2d scale(Vector2d vector, double scalar) {
        double newX = vector.x * scalar;
        double newY = vector.y * scalar;

        return new Vector2d(newX, newY);
    }

    /**
     * Normalizes the vector, making its length 1.
     *
     * Returns a null vector if the length of vector is 0;
     *
     * @param vector The vector to normalize.
     *
     * @return A new vector that is the normalized form of vector.
     */
    public static Vector2d normalize(Vector2d vector) {
        double length = vector.getLength();

        double newX = 0;
        double newY = 0;

        if (length != 0) {
            newX = vector.x / length;
            newY = vector.y / length;
        }

        return new Vector2d(newX, newY);
    }

    /**
     * Rotates vector by radians radians.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param vector The vector to rotate.
     * @param radians The amount to rotate the vector by, in radians.
     *
     * @return A new vector that is the translation of vector.
     */
    public static Vector2d rotate(Vector2d vector, double radians) {
        double newX = vector.x * Math.cos(radians) - vector.y * Math.sin(radians);
        double newY = vector.x * Math.sin(radians) + vector.y * Math.cos(radians);

        return new Vector2d(newX, newY);
    }
}