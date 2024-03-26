package me.trial.gang;

import org.bukkit.Location;
import org.bukkit.Particle;
import org.bukkit.World;

class Tornado {
    private double x;
    private double y;
    private double z;
    private int height;
    private int density = 1;
    private World countryRoads;

    Tornado(World world, double x, double y, double z) {
        this(world, x, y, z, 5);
    }

    Tornado(World world, double x, double y, double z, int height) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.height = height;
        countryRoads = world;
    }

    Tornado(Location loc, int height) {
        this.x = loc.getX();
        this.y = loc.getY();
        this.z = loc.getZ();
        this.height = height;
        countryRoads = loc.getWorld();
    }

    void runTornado(Particle particle, boolean counterClockwise) {
        Location loc = getLocation();
        Location trigLoc;
        double angleIncr;
        if (counterClockwise)
            for (int r = 0; r < height; r++) {
                loc.setX(loc.getX() + Math.random());
                loc.setY(loc.getY() + Math.random());
                loc.setZ(loc.getZ() + Math.random());

                angleIncr = (double) 360 / density + r;
                for (int k = 0; k < density + r; k++) {
                    trigLoc = loc;
                    trigLoc.setX(trigLoc.getX() + ((Math.sin(k * angleIncr)) * (r + 1)));
                    trigLoc.setY((trigLoc.getY() + r) + (Math.sin(k * angleIncr) / 2));
                    trigLoc.setZ(trigLoc.getZ() + ((Math.cos(k * angleIncr)) * (r + 1)));

                    countryRoads.spawnParticle(particle, trigLoc, 1, 0.1, 0.1, 0.1, 0);
                }
            }
        else
            for (int r = 0; r < height; r++) {
                loc.setX(loc.getX() + Math.random());
                loc.setY(loc.getY() + Math.random());
                loc.setZ(loc.getZ() + Math.random());

                angleIncr = (double) 360 / density + r;
                for (int k = 0; k < density + r; k++) {
                    trigLoc = loc;
                    trigLoc.setX(trigLoc.getX() + ((Math.cos(k * angleIncr)) * (r + 1)));
                    trigLoc.setY((trigLoc.getY() + r) + (Math.sin(k * angleIncr) / 2));
                    trigLoc.setZ(trigLoc.getZ() + ((Math.sin(k * angleIncr)) * (r + 1)));

                    countryRoads.spawnParticle(particle, trigLoc, 1, 0.1, 0.1, 0.1, 0);
                }
            }
    }

    void moveTornado(double deltaX, double deltaY, double deltaZ) {
        this.x += deltaX;
        this.y += deltaY;
        this.z += deltaZ;
    }

    void moveTornadoTo(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    void moveTornadoTo(Location loc) {
        this.x = loc.getX();
        this.y = loc.getY();
        this.z = loc.getZ();
    }

    void setHeight(int height) {
        this.height = height;
    }

    void setDensity(int density) {
        this.density = density;
    }

    double getX() {
        return x;
    }
    double getY() {
        return y;
    }
    double getZ() {
        return z;
    }
    Location getLocation() {
        return new Location(countryRoads, x, y, z);
    }
    World getWorld() {
        return countryRoads;
    }
}
