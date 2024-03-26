package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

/**
 * Code to help with writing custom commands.
 */
public final class CommandHelper {
    /**
     * Gets the targets of a command using the selector given.
     *
     * @param sender The sender of the command.
     * @param selector The selector to use.
     *
     * @return A list of entities fitting the selector.
     */
    public static List<Entity> getCommandTargets(CommandSender sender, String selector) {
        List<Entity> targets = new ArrayList<>();

        // Tries to get entities using a selector like @a.
        try {
            targets.addAll(Bukkit.selectEntities(sender, selector));

        } catch (IllegalArgumentException ignore) {}

        // Tries to get entities using a UUID.
        if (targets.isEmpty())
            try {
                Entity target = Bukkit.getEntity(UUID.fromString(selector));

                if (target != null)
                    targets.add(target);

            } catch (IllegalArgumentException ignored) {}

        // Tries to get entities using a player name.
        if (targets.isEmpty()) {
            Player target = Bukkit.getPlayerExact(selector);

            if (target != null)
                targets.add(target);
        }

        return targets;
    }



    /**
     * Gets all entities within a sphere with radius blocks of the location.
     *
     * @param location The location the sphere is centered around.
     * @param radius The radius of the sphere.
     * @param excludedEntities The entities to exclude from the search.
     *
     * @return A list of entities within radius blocks of location.
     */
    public static List<Entity> getEntitiesWithinRadius(Location location, double radius, List<Entity> excludedEntities) {
        Collection<Entity> nearbyEntities = location.getWorld().getNearbyEntities(location, radius, radius, radius);
        List<Entity> entitiesInRadius = new ArrayList<>();
        double radiusSquared = radius * radius;

        if (excludedEntities != null) {
            for (Entity entity : nearbyEntities)
                if (!excludedEntities.contains(entity) && entity.getLocation().distanceSquared(location) <= radiusSquared)
                    entitiesInRadius.add(entity);

        } else
            for (Entity entity : nearbyEntities)
                if (entity.getLocation().distanceSquared(location) <= radiusSquared)
                    entitiesInRadius.add(entity);

        return entitiesInRadius;
    }


    /**
     * Gets all entities within a sphere with radius blocks of (x,y,z).
     *
     * @param location The location the sphere is centered around.
     * @param radius The radius of the sphere.
     *
     * @return A list of entities within radius blocks of location.
     */
    public static List<Entity> getEntitiesWithinRadius(Location location, double radius) {
        return getEntitiesWithinRadius(location, radius, null);
    }

    /**
     * Gets all entities within a sphere with radius radius of the location.
     *
     * @param world The world to check for entities in.
     * @param x The x position of the sphere the search is centered around.
     * @param y The y position of the sphere the search is centered around.
     * @param z The z position of the sphere the search is centered around.
     * @param radius The radius of the sphere.
     * @param excludedEntities The entities to exclude from the search.
     *
     * @return A list of entities within radius blocks of point (x,y,z).
     */
    public static List<Entity> getEntitiesWithinRadius(World world, double x, double y, double z, double radius, List<Entity> excludedEntities) {
        return getEntitiesWithinRadius(new Location(world, x, y, z), radius, excludedEntities);
    }

    /**
     * Gets all entities within a sphere with radius blocks of (x,y,z).
     *
     * @param world The world to check for entities in.
     * @param x The x position of the sphere the search is centered around.
     * @param y The y position of the sphere the search is centered around.
     * @param z The z position of the sphere the search is centered around.
     * @param radius The radius of the sphere.
     *
     * @return A list of entities within radius blocks of point (x,y,z).
     */
    public static List<Entity> getEntitiesWithinRadius(World world, double x, double y, double z, double radius) {
        return getEntitiesWithinRadius(new Location(world, x, y, z), radius, null);
    }
}
