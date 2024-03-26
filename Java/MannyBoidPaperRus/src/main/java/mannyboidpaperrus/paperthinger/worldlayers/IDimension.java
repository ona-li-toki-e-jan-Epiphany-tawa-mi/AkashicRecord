package mannyboidpaperrus.paperthinger.worldlayers;

import mannyboidpaperrus.paperthinger.standardstuffs.SixAxisDirections;
import org.bukkit.World;
import org.bukkit.entity.Entity;

public interface IDimension {
    /**
     * Generates the world.
     */
    void generateWorld();

    /**
     * Tests whether or not the world exists.
     *
     * @return Whether or not the world exists.
     */
    boolean exists();

    /**
     * Gets the world as a world object;
     *
     * @return ZA WARUDO!
     */
    World getWorld();

    /**
     * Gets the size scaling of the world.
     *
     * @return The size scaling factor.
     */
    double getScalar();

    /**
     * Gets the name of the world.
     *
     * @return The name of the world.
     */
    String getName();

    /**
     * Called when an entity enters a world through void hopping.
     *
     * @param entity The entity that entered the world.
     * @param direction The direction that they came in from (up or down.)
     */
    default void onEntry(Entity entity, SixAxisDirections direction) {}

    /**
     * Called when an entity exits a world through void hopping.
     *
     * @param entity The entity that exited the world.
     * @param direction The direction that they are going (up or down.)
     */
    default void onExit(Entity entity, SixAxisDirections direction) {}
}
