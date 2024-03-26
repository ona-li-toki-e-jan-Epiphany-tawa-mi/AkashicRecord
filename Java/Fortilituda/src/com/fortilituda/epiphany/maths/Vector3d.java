package com.fortilituda.epiphany.maths;

/**
 * A 3d vector, filled with vector math methods.
 */
public class Vector3d extends Vector2d {
    private double x, y, z;

    /**
     * Creates a new null 3d vector.
     */
    public Vector3d() {
        x = 0;
        y = 0;
        z = 0;
    }

    /**
     * Creates a new 3d vector.
     *
     * @param x The x component of the vector.
     * @param y The y component of the vector.
     * @param z The z component of the vector.
     */
    public Vector3d(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Creates a copy of vector.
     *
     * @param vector The vector to copy.
     */
    public Vector3d(Vector3d vector) {
        x = vector.x;
        y = vector.y;
        z = vector.z;
    }

    public double getZ() {
        return z;
    }

    public void setZ(double z) {
        this.z = z;
    }

    /**
     * Translates the vector by (deltaX, deltaY, deltaZ.)
     *
     * Mutates the current vector.
     *
     * @param deltaX The amount to change the x component by.
     * @param deltaY The amount to change the y component by.
     * @param deltaZ The amount to change the z component by.
     *
     * @return Itself, for chaining.
     */
    public Vector3d translate(double deltaX, double deltaY, double deltaZ) {
        x += deltaX;
        y += deltaY;
        z += deltaZ;

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
    public Vector3d add(Vector3d otherVector) {
        x += otherVector.x;
        y += otherVector.y;
        z += otherVector.z;

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
    public Vector3d subtract(Vector3d otherVector) {
        x -= otherVector.x;
        y -= otherVector.y;
        z -= otherVector.z;

        return this;
    }

    @Override
    public Vector3d scale(double scalar) {
        x *= scalar;
        y *= scalar;
        z *= scalar;

        return this;
    }

    @Override
    public double getLengthSquared() {
        return x * x + y * y + z * z;
    }

    @Override
    public double getLength() {
        return Math.sqrt(x * x + y * y + z * z);
    }

    @Override
    public Vector3d normalize() {
        double length = getLength();

        if (length != 0) {
            x /= length;
            y /= length;
            z /= length;
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
    public double getDotProduct(Vector3d otherVector) {
        return x * otherVector.x + y * otherVector.y + z * otherVector.z;
    }

    /**
     * Gets the cross product of the current vector and otherVector.
     *
     * @param otherVector The other vector to use to find the cross product.
     *
     * @return The cross product of the current vector and otherVector (a vector.)
     */
    public Vector3d getCrossProduct(Vector3d otherVector) {
        return new Vector3d(
                y * otherVector.z - z * otherVector.y,
                z * otherVector.x - x * otherVector.z,
                x * otherVector.y - y * otherVector.x
        );
    }

    /**
     * Rotates the current vector along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * Mutates the current vector.
     *
     * @param xRotation_radians The amount to rotate the vector by along the x axis, in radians.
     * @param yRotation_radians The amount to rotate the vector by along the y axis, in radians.
     * @param zRotation_radians The amount to rotate the vector by along the z axis, in radians.
     *
     * @return Itself, for chaining.
     */
    public Vector3d rotate(double xRotation_radians, double yRotation_radians, double zRotation_radians) {
        double oldX = x;
        double oldY = y;
        double oldZ = y;

        if (xRotation_radians != 0) {
           y = oldY * Math.cos(xRotation_radians) - oldZ * Math.sin(xRotation_radians);
           z = oldY * Math.sin(xRotation_radians) + oldZ * Math.cos(xRotation_radians);

           oldY = y;
           oldZ = z;
        }

        if (yRotation_radians != 0) {
            x = oldX * Math.cos(xRotation_radians) + oldZ * Math.sin(xRotation_radians);
            z = oldZ * Math.cos(xRotation_radians) - oldX * Math.cos(xRotation_radians);

            oldX = x;
        }

        if (zRotation_radians != 0) {
            x = oldX * Math.cos(xRotation_radians) - oldY * Math.sin(xRotation_radians);
            y = oldX * Math.sin(xRotation_radians) + oldY * Math.cos(xRotation_radians);
        }

        return this;
    }

    /**
     * Rotates the current vector along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * Mutates the current vector.
     *
     * @param rotationVector A vector representing the various angles of rotation.
     *
     * @return Itself, for chaining.
     */
    public Vector3d rotate(Vector3d rotationVector) {
        return rotate(rotationVector.x, rotationVector.y, rotationVector.z);
    }

    @Override
    public String toString() {
        return "[" + x + ", " + y + ", " + z + "]";
    }

    /**
     * Translates the vector by (deltaX, deltaY, deltaZ.)
     *
     * @param vector The vector to translate.
     * @param deltaX The amount to change the x component by.
     * @param deltaY The amount to change the y component by.
     * @param deltaZ The amount to change the z component by.
     *
     * @return A new vector that is the translation of vector.
     */
    public static Vector3d translate(Vector3d vector, double deltaX, double deltaY, double deltaZ) {
        double newX = vector.x + deltaX;
        double newY = vector.y + deltaY;
        double newZ = vector.z + deltaZ;

        return new Vector3d(newX, newY, newZ);
    }

    /**
     * Adds otherVector to vector.
     *
     * @param vector The vector to add to.
     * @param otherVector The vector to add with.
     *
     * @return A new vector that is the sum of vector and otherVector.
     */
    public static Vector3d add(Vector3d vector, Vector3d otherVector) {
        double newX = vector.x + otherVector.x;
        double newY = vector.y + otherVector.y;
        double newZ = vector.z + otherVector.z;

        return new Vector3d(newX, newY, newZ);
    }

    /**
     * Subtracts otherVector from vector.
     *
     * @param vector The vector to subtract from.
     * @param otherVector The vector to subtract with.
     *
     * @return A new vector that is the difference of vector and otherVector.
     */
    public static Vector3d subtract(Vector3d vector, Vector3d otherVector) {
        double newX = vector.x - otherVector.x;
        double newY = vector.y - otherVector.y;
        double newZ = vector.z - otherVector.z;

        return new Vector3d(newX, newY, newZ);
    }

    /**
     * Scales the current vector by scalar.
     *
     * @param vector The vector to scale.
     * @param scalar The amount to scale by.
     *
     * @return A new vector that is the scaled form of vector.
     */
    public static Vector3d scale(Vector3d vector, double scalar) {
        double newX = vector.x * scalar;
        double newY = vector.y * scalar;
        double newZ = vector.z * scalar;

        return new Vector3d(newX, newY, newZ);
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
    public static Vector3d normalize(Vector3d vector) {
        double length = vector.getLength();

        double newX = 0;
        double newY = 0;
        double newZ = 0;

        if (length != 0) {
            newX = vector.x / length;
            newY = vector.y / length;
            newZ = vector.z / length;
        }

        return new Vector3d(newX, newY, newZ);
    }

    /**
     * Rotates the current vector along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param vector The vector to normalize.
     * @param xRotation_radians The amount to rotate the vector by along the x axis, in radians.
     * @param yRotation_radians The amount to rotate the vector by along the y axis, in radians.
     * @param zRotation_radians The amount to rotate the vector by along the z axis, in radians.
     *
     * @return A new vector that is the rotation of vector.
     */
    public static Vector3d rotate(Vector3d vector, double xRotation_radians, double yRotation_radians, double zRotation_radians) {
        Vector3d newVector = new Vector3d(vector.x, vector.y, vector.z);
        double oldX = newVector.x;
        double oldY = newVector.y;
        double oldZ = newVector.y;

        if (xRotation_radians != 0) {
            newVector.y = oldY * Math.cos(xRotation_radians) - oldZ * Math.sin(xRotation_radians);
            newVector.z = oldY * Math.sin(xRotation_radians) + oldZ * Math.cos(xRotation_radians);

            oldY = newVector.y;
            oldZ = newVector.z;
        }

        if (yRotation_radians != 0) {
            newVector.x = oldX * Math.cos(xRotation_radians) + oldZ * Math.sin(xRotation_radians);
            newVector.z = oldZ * Math.cos(xRotation_radians) - oldX * Math.cos(xRotation_radians);

            oldX = newVector.x;
        }

        if (zRotation_radians != 0) {
            newVector.x = oldX * Math.cos(xRotation_radians) - oldY * Math.sin(xRotation_radians);
            newVector.y = oldX * Math.sin(xRotation_radians) + oldY * Math.cos(xRotation_radians);
        }

        return newVector;
    }

    /**
     * Rotates the current vector along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param vector The vector to normalize.
     * @param rotationVector A vector representing the angles of rotation.
     *
     * @return A new vector that is the rotation of vector.
     */
    public static Vector3d rotate(Vector3d vector, Vector3d rotationVector) {
       return rotate(vector, rotationVector.x, rotationVector.y, rotationVector.z);
    }
}
