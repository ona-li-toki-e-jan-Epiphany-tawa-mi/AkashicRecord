package mannyboidpaperrus.paperthinger.standardstuffs;

import com.sun.javafx.geom.Point2D;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

import java.util.List;
import java.util.Vector;

/**
 * Just some standard functions.
 */
public class MiscPaperFunctions {
    /**
     * Gets the highest block below a specified coordinate.
     * Takes into account transparent blocks if strongSelection is true.
     *
     * @param loc The location to check.
     * @param strongSelection Whether or not to ignore non-solid blocks.
     *
     * @return The location of the highest block below loc, or null, if no valid blocks can be found.
     */
    public static Location getHighestBlockBelow(Location loc, boolean strongSelection) {
        return getHighestBlockBelow(loc.getWorld(), loc.getBlockX(), loc.getBlockY(), loc.getBlockZ(), strongSelection);
    }

    /**
     * Gets the highest block below a specified coordinate.
     * Takes into account transparent blocks if strongSelection is true.
     *
     * @param world The world that the location to check is in.
     * @param x The x coordinate of the location to check.
     * @param y The y coordinate of the location to check.
     * @param z The z coordinate of the location to check.
     * @param strongSelection Whether or not to account for transparent blocks.
     *
     * @return The location of the highest block below (x, y, z), or null, if no valid blocks can be found.
     */
    public static Location getHighestBlockBelow(World world, int x, int y, int z, boolean strongSelection) {
        for (int y2 = y - 1; y2 >= 0; y2--) {
            boolean validBlock = true;

            if (strongSelection) {
                for (Material transparentBlock : MiscPaperConstants.transparentBlocks)
                    if (transparentBlock.equals(world.getBlockAt(x, y2, z).getType())) {
                        validBlock = false;
                        break;
                    }
            } else
                for (Material emptyBlock : MiscPaperConstants.emptyBlocks)
                    if (emptyBlock.equals(world.getBlockAt(x, y2, z).getType())) {
                        validBlock = false;
                        break;
                    }

            if (validBlock)
                return new Location(world, x, y2, z);
        }

        return null;
    }

    /**
     * Finds whether or not entity is below (x, y, z) with nothing in the way.
     * Takes into account transparent blocks if strongSelection is true.
     * Fails if entity#getWorld() != world.
     *
     * @param entity The entity to test for being below (x, y, z).
     * @param location The location to check with.
     * @param strongSelection Whether or not to account for transparent blocks.
     *
     * @return Whether or not entity is below (x, y, z) with nothing in the way.
     */
    public static boolean isEntityExposedTo(Entity entity, Location location, boolean strongSelection) {
        return isEntityExposedTo(entity, location.getWorld(), location.getBlockX(), location.getBlockY(), location.getBlockZ(), strongSelection);
    }

    /**
     * Finds whether or not entity is below (x, y, z) with nothing in the way.
     * Takes into account transparent blocks if strongSelection is true.
     * Fails if entity#getWorld() != world.
     *
     * @param entity The entity to test for being below (x, y, z).
     * @param world The world to do the check in.
     * @param x The x coordinate of the location to check.
     * @param y The y coordinate of the location to check.The x coordinate of the location to check.
     * @param z The z coordinate of the location to check.The x coordinate of the location to check.
     * @param strongSelection Whether or not to account for transparent blocks.
     *
     * @return Whether or not entity is below (x, y, z) with nothing in the way.
     */
    public static boolean isEntityExposedTo(Entity entity, World world, int x, int y, int z, boolean strongSelection) {
        if (entity.getWorld() != world)
            return false;

        if (entity.getLocation().getBlockX() <= x - 0.5 || entity.getLocation().getBlockX() >= x + 0.5)
            return false;

        if (entity.getLocation().getBlockZ() <= z - 0.5 || entity.getLocation().getBlockX() >= z + 0.5)
            return false;

        if (entity.getLocation().getBlockY() > y)
            return false;

        for (int y2 = entity.getLocation().getBlockY(); y2 < y; y2++) {
            boolean invalidBlock;
            boolean valid = false;

            if (strongSelection) {
                for (Material transparentBlock : MiscPaperConstants.transparentBlocks)
                    if (transparentBlock.equals(world.getBlockAt(x, y2, z).getType())) {
                        valid = true;
                        break;
                    }

                invalidBlock = !valid;
            } else {
                for (Material emptyBlock : MiscPaperConstants.emptyBlocks)
                    if (emptyBlock.equals(world.getBlockAt(x, y2, z).getType())) {
                        valid = true;
                        break;
                    }

                invalidBlock = !valid;
            }

            if (invalidBlock)
                return false;
        }

        return true;
    }

    // TODO Add operator arguments (square brackets).
    @Deprecated
    public static List<Entity> getEntitiesFromSelector(Entity executor, String selector) {
        List<Entity> entities = new Vector<>();
        Location location = executor.getLocation();

        if (selector.length() >= 2 && selector.charAt(0) == '@') {
            // @a
            if (selector.toLowerCase().charAt(1) == 'a') {
                for (World world : Bukkit.getWorlds())
                    for (Entity entity : world.getEntities())
                        if (entity instanceof Player)
                            entities.add(entity);

            // @e
            } else if (selector.toLowerCase().charAt(1) == 'e') {
                for (World world : Bukkit.getWorlds())
                    entities.addAll(world.getEntities());

            // @p
            } else if (selector.toLowerCase().charAt(1) == 'p') {
                double distance = Double.MAX_VALUE;
                Entity closestPlayer = null;

                for (World world : Bukkit.getWorlds())
                    for (Entity entity : world.getEntities())
                        if (entity instanceof Player && location.distanceSquared(entity.getLocation()) < distance) {
                            distance = location.distanceSquared(entity.getLocation());
                            closestPlayer = entity;
                        }

                if (closestPlayer != null)
                    entities.add(closestPlayer);

            // @r
            } else if (selector.toLowerCase().charAt(1) == 'r') {
                Vector<Entity> players = new Vector<>();

                for (World world : Bukkit.getWorlds())
                    for (Entity entity : world.getEntities())
                        if (entity instanceof Player)
                            players.add(entity);

                if (players.size() > 0) {
                    int randomPlayer = (int) (players.size() * Math.random());
                    entities.add(players.elementAt(randomPlayer));
                }

            // @s
            } else if (selector.toLowerCase().charAt(1) == 's')
                entities.add(executor);
        }

        // Tries name, custom name, and UUID if nothing else yielded results.
        if (entities.size() == 0)
            for (World world : Bukkit.getWorlds())
                for (Entity entity : world.getEntities())
                    if (entity.getName().equalsIgnoreCase(selector) || (entity.getCustomName() != null && entity.getCustomName().equalsIgnoreCase(selector))
                        || entity.getUniqueId().toString().equalsIgnoreCase(selector))
                        entities.add(entity);

        return entities;
    }

    /**
     * Gets entities from a selector by hijacking the games normal command system to tag applicable entities.
     *
     * @param executor The entity executing the command.
     * @param selector The selector filtering possible victims.
     *
     * @return The entities found.
     */
    public static List<Entity> hijackEntitiesFromSelector(Entity executor, String selector) {
        List<Entity> entities = new Vector<>();

        Bukkit.dispatchCommand(executor, "tag " + selector + " add paperthinger_commandhijack");

        for (World world : Bukkit.getWorlds())
            for (Entity entity : world.getEntities())
                if (entity.getScoreboardTags().contains("paperthinger_commandhijack")) {
                    entities.add(entity);
                    entity.getScoreboardTags().remove("paperthinger_commandhijack");
                }

        return entities;
    }

    /**
     * Converts chunk coordinates into cartesian coordinates.
     *
     * @param chunkX The x position of the chunk.
     * @param chunkZ The z position of the chunk.
     * @param relativeX The relative x position inside the chunk.
     * @param relativeZ The relative z position inside the chunk.
     *
     * @return The cartesian coordinate equivalent.
     */
    public static Point2D getPointFromChunkCoordinates(int chunkX, int chunkZ, int relativeX, int relativeZ) {
        int actualX;
        int actualZ;

        if (chunkX >= 0) {
            actualX = (chunkX << 4) + relativeX;

        } else
            actualX = ((chunkX + 1) << 4) + relativeX - 16;

        if (chunkZ >= 0) {
            actualZ = (chunkZ << 4) + relativeZ;

        } else
            actualZ = ((chunkZ + 1) << 4) + relativeZ - 16;

        return new Point2D(actualX, actualZ);
    }
}
