package mannyboidpaperrus.paperthinger.worldlayers;

import mannyboidpaperrus.paperthinger.PaperThinger;
import mannyboidpaperrus.paperthinger.standardstuffs.SixAxisDirections;
import mannyboidpaperrus.paperthinger.worldlayers.voidd.Void;
import org.bukkit.*;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerBedEnterEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.generator.ChunkGenerator;
import org.bukkit.inventory.ItemStack;

import java.util.Vector;

// Horizontal dimensions acting as alternate versions of normal ones. Access somehow by world border on north and south ends.
// Add limbo or backrooms as random chance for someone to go to when travelling by voids.
// Add world of randomness that has weird biomes and occasionally resets chunks. Also it puts random shapes on held maps.
// Add amulet of dichotomous recoil that causes the wearer to bounce off of voids, whether below -64 or above 319.
// Add the min and max heights of the worlds to IDimension and force void hopping to use it, default to -64, 319.

// Add way to add extra dimensions to the chain on the fly.

// Possible bad effects when going up like in "Made in Abyss."

/**
 * Manages Dimensions and facilitates void hopping.
 */
public class DimensionManager implements Listener {
    // It's turtles all the way down.
    public static final Vector<World> dimensions = new Vector<>();
    public static final Vector<Double> scalingFactor = new Vector<>();
    public static final Vector<IDimension> customDimensionList = new Vector<>();

    public static void startUp() {
        addDimension(Bukkit.getWorld("world_nether"), 8);
        addDimension(Bukkit.getWorld("world"), 1);
        addDimension(Bukkit.getWorld("world_the_end"), 1);

        Void theVoid = new Void();
        if (!theVoid.exists())
            theVoid.generateWorld();
        addDimensionAtIndex(0, theVoid);
        Bukkit.getPluginManager().registerEvents(theVoid, PaperThinger.getInstance());
    }

    /**
     * Runs over entities.
     *
     * @param world The world entity is in.
     * @param entity The entity currently being checked on the loop.
     */
    public static void entityLoop(World world, Entity entity, byte ticks, double deltaTime) {
        Location loc = entity.getLocation();

        if (loc.getY() < -64) {
            int worldIndex = dimensions.indexOf(world);

            // Sends entities to a lower world.
            if (worldIndex != -1 && worldIndex != 0) {
                // Normalizes entity x & z positions.
                loc.setX(loc.getX() * scalingFactor.get(worldIndex));
                loc.setZ(loc.getZ() * scalingFactor.get(worldIndex));
                // And scales accordingly.
                loc.setX(loc.getX() / scalingFactor.get(worldIndex - 1));
                loc.setZ(loc.getZ() / scalingFactor.get(worldIndex - 1));

                if (customDimensionList.get(worldIndex) != null)
                    customDimensionList.get(worldIndex).onExit(entity, SixAxisDirections.DOWN);

                shiftEntity(entity, dimensions.get(worldIndex - 1), loc.getX(), world.getMaxHeight() + 64, loc.getZ());

                if (customDimensionList.get(worldIndex - 1) != null)
                    customDimensionList.get(worldIndex - 1).onEntry(entity, SixAxisDirections.DOWN);
            }

        } else if (loc.getY() > world.getMaxHeight() + 64) {
            int worldIndex = dimensions.indexOf(world);

            // Sends entities to a higher world.
            if (worldIndex != -1 && worldIndex != dimensions.size() - 1) {
                // Normalizes entity x & z positions.
                loc.setX(loc.getX() * scalingFactor.get(worldIndex));
                loc.setZ(loc.getZ() * scalingFactor.get(worldIndex));
                // And scales accordingly.
                loc.setX(loc.getX() / scalingFactor.get(worldIndex + 1));
                loc.setZ(loc.getZ() / scalingFactor.get(worldIndex + 1));

                if (customDimensionList.get(worldIndex) != null)
                    customDimensionList.get(worldIndex).onExit(entity, SixAxisDirections.UP);

                shiftEntity(entity, dimensions.get(worldIndex + 1), loc.getX(), -64, loc.getZ());

                if (customDimensionList.get(worldIndex + 1) != null)
                    customDimensionList.get(worldIndex + 1).onEntry(entity, SixAxisDirections.UP);
            }
        }

        Void.entityLoop(world, entity, ticks, deltaTime);
    }

    /**
     * Runs.
     *
     * @param timePassed The amount of time that has passed.
     */
    public static void loop(double timePassed, byte ticks) {
        Void.loop(timePassed, ticks);
    }

    /**
     * A unified function to create extra worlds.
     *
     * @param name The name of the world to be created.
     * @param generator The generator to be used when generating the world.
     * @param environment The overall environment of the generated world (Overworld, Nether, or End).
     * @param generateStructures Whether or not to generate structures in the world.
     */
    public static void createWorld(String name, ChunkGenerator generator, World.Environment environment, boolean generateStructures) {
        WorldCreator worldCreator = new WorldCreator(name);

        worldCreator.generator(generator);
        worldCreator.generateStructures(generateStructures);
        worldCreator.environment(environment);

        worldCreator.createWorld();
    }

    /**
     * Shifts entities to other worlds (or dimensions).
     * Preserves velocity between shifts.
     *
     * @param entity The entity to be shifted.
     * @param worldTo The world (or dimension) that the entity is shifted to.
     * @param x The new x position of the entity.
     * @param y The new y position of the entity.
     * @param z The new z position of the entity.
     */
    private static void shiftEntity(Entity entity, World worldTo, double x, double y, double z) {
        org.bukkit.util.Vector velocity = entity.getVelocity();
        Location loc = entity.getLocation();

        loc.setWorld(worldTo);
        loc.set(x, y, z);

        entity.teleport(loc);
        entity.setVelocity(velocity);
    }

    /**
     * Adds a world to the dimension list used for void hopping.
     *
     * @param world The world to add to the dimension list.
     * @param scalar The scaling factor of the size of world.
     */
    private static void addDimension(World world, double scalar) {
        dimensions.add(world);
        scalingFactor.add(scalar);
        customDimensionList.add(null);
    }

    /**
     *  Adds a world to the dimension list used for void hopping.
     *
     * @param dimensionalInstance An instance of IDimension that represents the custom dimension.
     */
    private static void addDimension(IDimension dimensionalInstance) {
        dimensions.add(dimensionalInstance.getWorld());
        scalingFactor.add(dimensionalInstance.getScalar());
        customDimensionList.add(dimensionalInstance);
    }

    /**
     * Adds a world to the dimension list used for void hopping, at a specified index.
     *
     * @param world The world to add to the dimension list.
     * @param scalar The scaling factor of the size of world.
     * @param index The index where the world and scalar should go.
     */
    private static void addDimensionAtIndex(int index, World world, double scalar) {
        dimensions.add(index, world);
        scalingFactor.add(index, scalar);
        customDimensionList.add(index, null);
    }

    /**
     * Adds a world to the dimension list used for void hopping, at a specified index.
     *
     * @param dimensionalInstance An instance of IDimension representing the custom dimension
     */
    private static void addDimensionAtIndex(int index, IDimension dimensionalInstance) {
        dimensions.add(index, dimensionalInstance.getWorld());
        scalingFactor.add(index, dimensionalInstance.getScalar());
        customDimensionList.add(index, dimensionalInstance);
    }

    /**
     * Prevents end portals from being created in the end and end related dimensions.
     * Going through one in those conditions causes some... issues...
     *
     * @param pce An object containing details of a miscellaneous player interaction.
     */
    @EventHandler
    public void preventEndPortalsInEnd(PlayerInteractEvent pce) {
        Player player = pce.getPlayer();
        World world = player.getWorld();

        if (world.getEnvironment() == World.Environment.THE_END) {
            Action playerAction = pce.getAction();
            Block clickedBlock = pce.getClickedBlock();
            ItemStack heldItem = pce.getItem();

            if (playerAction.equals(Action.RIGHT_CLICK_BLOCK) && clickedBlock != null && clickedBlock.getType().equals(Material.END_PORTAL_FRAME))
                if (heldItem != null && heldItem.getType().equals(Material.ENDER_EYE))
                    pce.setCancelled(true);
        }
    }

    /**
     * Prevents beds from exploding when players sleep in nether or end enviroments
     *
     * @param playerBedEnterEvent An object containing details of a miscellaneous player-bed interaction.
     */
    @EventHandler
    public void preventBedExplosion(PlayerBedEnterEvent playerBedEnterEvent) {
        Player player = playerBedEnterEvent.getPlayer();
        World.Environment environment = player.getWorld().getEnvironment();

        if (environment.equals(World.Environment.NETHER) || environment.equals(World.Environment.THE_END))
            playerBedEnterEvent.setUseBed(Event.Result.ALLOW);
    }
}
