package com.fortilituda.epiphany.maths.shapes;

import com.fortilituda.epiphany.maths.Vector3d;

/**
 * A mathematical representation of a cuboid, which is like a rectangular prism.
 */
public class Cuboid {
    private double width, length, height;
    private Vector3d position;
    private double xRot, yRot, zRot;
    private Vector3d[] vertices;

    public static int[] verticePairIndexes = {
            0, 4,
            4, 5,
            5, 7,
            7, 0,
            0, 3,
            4, 6,
            5, 1,
            7, 2,
            3, 6,
            6, 1,
            1, 2,
            2, 3
    };

    /**
     * Creates a unit cuboid, aka a unit cube.
     */
    public Cuboid() {
        width = 1;
        length = 1;
        height = 1;
        position = new Vector3d(0, 0, 0);
        xRot = 0;
        yRot = 0;
        zRot = 0;
        vertices = new Vector3d[8];

        updateVertices();
    }

    /**
     * Creates a unit cuboid, aka a unit cube, at (x, y, z.)
     *
     * @param x The x component of the cuboid's position.
     * @param y The y component of the cuboid's position.
     * @param z The z component of the cuboid's position.
     */
    public Cuboid(double x, double y, double z) {
        width = 1;
        length = 1;
        height = 1;
        position = new Vector3d(x, y, z);
        xRot = 0;
        yRot = 0;
        zRot = 0;
        vertices = new Vector3d[8];

        updateVertices();
    }

    /**
     * Creates a unit cuboid, aka a unit cube, at position.
     *
     * @param position A vector representing the cuboid's position.
     */
    public Cuboid(Vector3d position) {
        width = 1;
        length = 1;
        height = 1;
        this.position = new Vector3d(position);
        xRot = 0;
        yRot = 0;
        zRot = 0;
        vertices = new Vector3d[8];

        updateVertices();
    }

    /**
     * Creates a cuboid with dimensions of width * length * height at (x, y, z.)
     *
     * @param x The x component of the cuboid's position.
     * @param y The y component of the cuboid's position.
     * @param z The z component of the cuboid's position.
     * @param width The width of the cuboid.
     * @param length The length of the cuboid.
     * @param height The height of the cuboid.
     */
    public Cuboid(double x, double y, double z, double width, double length, double height) {
        this.width = width;
        this.length = length;
        this.height = height;
        position = new Vector3d(x, y, z);
        xRot = 0;
        yRot = 0;
        zRot = 0;
        vertices = new Vector3d[8];

        updateVertices();
    }

    /**
     * Creates a cuboid with dimensions of dimensions at position.
     *
     * @param position A vector representing the cuboid's position.
     * @param dimensions A vector representing the dimensions of the cuboid.
     */
    public Cuboid(Vector3d position, Vector3d dimensions) {
        width = dimensions.getX();
        length = dimensions.getY();
        height = dimensions.getZ();
        this.position = new Vector3d(position);
        xRot = 0;
        yRot = 0;
        zRot = 0;
        vertices = new Vector3d[8];

        updateVertices();
    }

    /**
     * Creates a cuboid with dimensions of width * length * height at (x, y, z,) rotated along the axes by xRot_radians, yRot_radians, and zRot_radians.
     *
     * @param x The x component of the cuboid's position.
     * @param y The y component of the cuboid's position.
     * @param z The z component of the cuboid's position.
     * @param width The width of the cuboid.
     * @param length The length of the cuboid.
     * @param height The height of the cuboid.
     * @param xRot_radians The angle to rotate the cuboid about the x-axis, in radians.
     * @param yRot_radians The angle to rotate the cuboid about the y-axis, in radians.
     * @param zRot_radians The angle to rotate the cuboid about the z-axis, in radians.
     */
    public Cuboid(double x, double y, double z, double width, double length, double height, double xRot_radians, double yRot_radians, double zRot_radians) {
        this.width = width;
        this.length = length;
        this.height = height;
        position = new Vector3d(x, y, z);
        xRot = xRot_radians;
        yRot = yRot_radians;
        zRot = zRot_radians;
        vertices = new Vector3d[8];

        updateVertices();
    }

    /**
     * Creates a cuboid with dimensions of dimensions at position, rotated along the axes by rotation.
     *
     * @param position A vector representing the cuboid's position.
     * @param dimensions A vector representing the dimensions of the cuboid.
     * @param rotation A vector representing the rotation of the cuboid.
     */
    public Cuboid(Vector3d position, Vector3d dimensions, Vector3d rotation) {
        width = dimensions.getX();
        length = dimensions.getY();
        height = dimensions.getZ();
        this.position = new Vector3d(position);
        xRot = rotation.getX();
        yRot = rotation.getY();
        zRot = rotation.getZ();
        vertices = new Vector3d[8];

        updateVertices();
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getLength() {
        return length;
    }

    public void setLength(double length) {
        this.length = length;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    public Vector3d getPosition() {
        return new Vector3d(position);
    }

    public void setPosition(Vector3d position) {
        this.position = position;
    }

    public double getXRot() {
        return xRot;
    }

    public void setXRot(double xRot) {
        this.xRot = xRot;
    }

    public double getYRot() {
        return yRot;
    }

    public void setYRot(double yRot) {
        this.yRot = yRot;
    }

    public double getZRot() {
        return zRot;
    }

    public void setZRot(double zRot) {
        this.zRot = zRot;
    }

    public Vector3d[] getVertices() {
        return vertices;
    }

    /**
     * Updates the vertices of the cuboid to its current position and rotation.
     */
    public void updateVertices() {
        double halfWidth = width / 2;
        double halfLength = length / 2;
        double halfHeight = height / 2;

        vertices[0] = new Vector3d(halfWidth, halfLength, halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[1] = new Vector3d(-halfWidth, -halfLength, -halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[2] = new Vector3d(halfWidth, -halfLength, -halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[3] = new Vector3d(halfWidth, halfLength, -halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[4] = new Vector3d(-halfWidth, halfLength, halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[5] = new Vector3d(-halfWidth, -halfLength, halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[6] = new Vector3d(-halfWidth, halfLength, -halfHeight).rotate(xRot, yRot, zRot).add(position);
        vertices[7] = new Vector3d(halfWidth, -halfLength, halfHeight).rotate(xRot, yRot, zRot).add(position);
    }

    /**
     * Sets the width, length, and height of the cube.
     * Updates the vertices after the scale if updateVertices is true.
     *
     * @param width The new width of the cuboid.
     * @param length The new length of the cuboid.
     * @param height The new height of the cuboid.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid setDimensions(double width, double length, double height, boolean updateVertices) {
        this.width = width;
        this.length = length;
        this.height = height;

        if (updateVertices)
            updateVertices();

        return this;
    }

    /**
     * Sets the width, length, and height of the cube.
     * Updates the vertices after the scale if updateVertices is true.
     *
     * @param dimensionVector A vector representing the new dimensions of the cube.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid setDimensions(Vector3d dimensionVector, boolean updateVertices) {
        return setDimensions(dimensionVector.getX(), dimensionVector.getY(), dimensionVector.getZ(), updateVertices);
    }

    /**
     * Scales the size of the cuboid by scalar.
     * Updates the vertices after the scale if updateVertices is true.
     *
     * @param scalar The amount to scale the cuboid's size by.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid scale(double scalar, boolean updateVertices) {
        width *= scalar;
        length *= scalar;
        height *= scalar;

        if (updateVertices)
            updateVertices();

        return this;
    }

    /**
     * Sets the cuboid's length, width, and height to 1, making its area 1.
     * Updates the vertices after the normalization if updateVertices is true.
     *
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid normalize(boolean updateVertices) {
        width = 1;
        length = 1;
        height = 1;

        if (updateVertices)
            updateVertices();

        return this;
    }

    /**
     * Sets the rotation of the cuboid's vertices along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param xRot_radians The angle of rotation about the x-axis.
     * @param yRot_radians The angle of rotation about the y-axis.
     * @param zRot_radians The angle of rotation about the z-axis.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid setRotation(double xRot_radians, double yRot_radians, double zRot_radians, boolean updateVertices) {
        xRot = xRot_radians;
        yRot = yRot_radians;
        zRot = zRot_radians;

        if (updateVertices)
            updateVertices();

        return this;
    }

    /**
     * Sets the rotation of the cuboid's vertices along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param rotationVector A vector representing the various angles of rotation.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid setRotation(Vector3d rotationVector, boolean updateVertices) {
        return setRotation(rotationVector.getX(), rotationVector.getY(), rotationVector.getZ(), updateVertices);
    }

    /**
     * Rotates the cuboid's vertices along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param xRot_radians The angle to rotate about the x-axis.
     * @param yRot_radians The angle to rotate about the y-axis.
     * @param zRot_radians The angle to rotate about the z-axis.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid rotate(double xRot_radians, double yRot_radians, double zRot_radians, boolean updateVertices) {
        xRot += xRot_radians;
        yRot += yRot_radians;
        zRot += zRot_radians;

        double pi2 = Math.PI * 2;

        if (xRot != 0)
            if (xRot > pi2) {
                xRot %= pi2;

            } else if (xRot < -pi2)
                xRot = pi2 + (xRot % pi2);

        if (this.yRot != 0)
            if (this.yRot > pi2) {
                this.yRot %= pi2;

            } else if (yRot < -pi2)
                yRot = pi2 + (yRot % pi2);

        if (this.xRot != 0)
            if (zRot > pi2) {
                zRot %= pi2;

            } else if (zRot < -pi2)
                zRot = pi2 + (zRot % pi2);

        if (updateVertices)
            updateVertices();

        return this;
    }

    /**
     * Rotates the cuboid's vertices along the various axes.
     * Positive angles mean counter-clockwise rotation, negative angles mean clockwise rotation.
     *
     * @param rotationVector A vector representing the angles of rotation.
     * @param updateVertices Whether or not to update the vertices afterward.
     *
     * @return Itself, for chaining.
     */
    public Cuboid rotate(Vector3d rotationVector, boolean updateVertices) {
        return rotate(rotationVector.getX(), rotationVector.getY(), rotationVector.getZ(), updateVertices);
    }
}
