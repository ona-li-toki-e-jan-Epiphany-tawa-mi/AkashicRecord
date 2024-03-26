package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers;

import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.util.Vector;

/**
 * Code to help with general actions done to entities, like movement.
 */
public class EntityAffectors {
    /**
     * Teleports an entity to a location.
     * Preserves momentum.
     *
     * @param entity The entity to teleport.
     * @param location The location to teleport the entity to.
     */
    public static void retainingTeleport(Entity entity, Location location) {
        Vector velocity = entity.getVelocity();
        entity.teleport(location);
        entity.setVelocity(velocity);
    }
}
