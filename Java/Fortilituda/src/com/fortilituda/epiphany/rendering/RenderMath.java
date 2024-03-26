package com.fortilituda.epiphany.rendering;

import com.fortilituda.epiphany.maths.Vector2d;
import com.fortilituda.epiphany.maths.Vector3d;

/**
 * Some math methods required for rendering 3d objects to the screen.
 */
public class RenderMath {
    /**
     * Converts a 3d position in pube space (normalized 3d space) into a 2d position on screen space.
     * If the z coordinate is 0, it uses orthographic rendering, otherwise it uses stereographic rendering.
     *
     * @param x The x coordinate in pube space.
     * @param y The y coordinate in pube space.
     * @param z The z coordinate in pube space.
     * @param halfWidth Half of the width of screen space.
     * @param halfHeight Half of the height of screen space.
     *
     * @return The equivalent position in screen space.
     */
    public static Vector2d pubeSpaceToScreenSpace(double x, double y, double z, double halfWidth, double halfHeight) {
        double newX, newY;

        if (z == 0) {
            newX = x * halfWidth + halfWidth;
            newY = y * halfHeight + halfHeight;

        } else {
            newX = x / z * halfWidth + halfWidth;
            newY = y / z * halfHeight + halfHeight;
        }

        return new Vector2d(newX, newY);
    }

    /**
     * Converts a 3d position in pube space (normalized 3d space) into a 2d position on screen space.
     * If the z coordinate is 0, it uses orthographic rendering, otherwise it uses stereographic rendering.
     *
     * @param positionVector A vector representing the point to convert into screen space.
     * @param halfWidth Half of the width of screen space.
     * @param halfHeight Half of the height of screen space.
     *
     * @return The equivalent position in screen space.
     */
    public static Vector2d pubeSpaceToScreenSpace(Vector3d positionVector, double halfWidth, double halfHeight) {
        double newX, newY;

        if (positionVector.getZ() == 0) {
            newX = positionVector.getX() * halfWidth + halfWidth;
            newY = positionVector.getY() * halfHeight + halfHeight;

        } else {
            newX = positionVector.getX() / positionVector.getZ() * halfWidth + halfWidth;
            newY = positionVector.getY() / positionVector.getZ() * halfHeight + halfHeight;
        }

        return new Vector2d(newX, newY);
    }

    /**
     * Makes the camera the origin and its rotation the basis vectors for (x, y, z.)
     *
     * @param x The x position of the point to transform.
     * @param y The y position of the point to transform.
     * @param z The z position of the point to transform.
     * @param cameraX The x position of the camera.
     * @param cameraY The y position of the camera.
     * @param cameraZ The z position of the camera.
     * @param cameraXRot The camera's rotation about the x axis.
     * @param cameraYRot The camera's rotation about the y axis.
     * @param cameraZRot The camera's rotation about the z axis.
     *
     * @return The transformed vector.
     */
    public static Vector3d alignBasisVectorsToCamera(double x, double y, double z, double cameraX, double cameraY, double cameraZ, double cameraXRot, double cameraYRot, double cameraZRot) {
        return new Vector3d(x, y, z).translate(-cameraX, -cameraY, -cameraZ).rotate(cameraXRot, cameraYRot, cameraZRot);
    }

    /**
     * Makes the camera the origin and its rotation the basis vectors for (x, y, z.)
     *
     * @param positionVector A vector representing the point to transform.
     * @param cameraPositionVector A vector representing the camera's position.
     * @param cameraRotationVector A vector representing the camera's rotation about the various axes.
     *
     * @return The transformed vector.
     */
    public static Vector3d alignBasisVectorsToCamera(Vector3d positionVector, Vector3d cameraPositionVector, Vector3d cameraRotationVector) {
        return Vector3d.subtract(positionVector, cameraPositionVector).rotate(cameraRotationVector);
    }
}
