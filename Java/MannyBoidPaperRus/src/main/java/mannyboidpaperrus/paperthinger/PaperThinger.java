package mannyboidpaperrus.paperthinger;

import mannyboidpaperrus.paperthinger.bleblocks.BlockHandler;
import mannyboidpaperrus.paperthinger.items.CustomItemManager;
import mannyboidpaperrus.paperthinger.items.ItemHandler;
import mannyboidpaperrus.paperthinger.mobbs.MobHandler;
import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperFunctions;
import mannyboidpaperrus.paperthinger.worldlayers.DimensionManager;
import org.bukkit.Bukkit;
import org.bukkit.Color;
import org.bukkit.World;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.event.Listener;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitRunnable;

import java.util.List;

/**
 * An epic plugin for epic gamers, just how I like it.
 */
public final class PaperThinger extends JavaPlugin implements Listener {
    private static PaperThinger instance;

    // Make Kikoku.
    // Make stuff scarier by improving mob AI.
    // Make it so only flammable items burn up in lava and fire. (make tnt explode as an item, make it a small blast radius (and gunpowder)).
    //  Also make it so certain items survive tnt explosions.
    //  Have food cook whilst on fire.
    //  Have cooked food and other food items char into dried kelp and call it "Scorched Food."
    //  Have sponges dry in the heat.
    //  Have lava smelt items.
    // Cocoa beans act like gravity blocks when not attached, falling cocoa beans inflict damage like anvils. Have arrows knock off cocoa pods.
    // Add holy hand grenade.
    // Add Brainfuck interpreter that players write in books.
    // Add black hole.
    //  Figure out how to put relativity in minecraft. (hint: players act as dark energy).

    @Override
    public void onEnable() {
        instance = this;

        getLogger().info("Shades: on.");

        DimensionManager.startUp();
        CustomItemManager.startUp();

        getLogger().info("Doritos: monched.");

        getServer().getPluginManager().registerEvents(instance, this);
        getServer().getPluginManager().registerEvents(new DimensionManager(), this);
        getServer().getPluginManager().registerEvents(new MobHandler(), this);
        getServer().getPluginManager().registerEvents(new BlockHandler(), this);
        getServer().getPluginManager().registerEvents(new ItemHandler(), this);

        getLogger().info("Adderal: consumed.");

        new BukkitRunnable() { @Override public void run() {
            periodic();
        }}.runTaskTimer(this, 0, 1);

        getLogger().info("Oh yeah, it's Epic Gamer time.");
    }

    @Override
    public void onDisable() {}

    private double timePassed = 0;
    private long lastTime = System.nanoTime();
    private byte ticks = 0;
    /**
     * Keeps track of how much time has passed since last execution.
     */
    private void periodic() {
        long currentTime = System.nanoTime();
        double dt = (currentTime - lastTime) / 1_000_000_000.0;

        mainLoop(dt);

        timePassed += dt;
        lastTime = System.nanoTime();

        ticks++;
        if (ticks < 0)
            ticks = 0;
    }

    /**
     * Runs.
     *
     * @param deltaTime The amount of time that has passed since last execution.
     */
    private void mainLoop(double deltaTime) {
        DimensionManager.loop(timePassed, ticks);

        for (World world : Bukkit.getWorlds())
            for (Entity entity : world.getEntities()) {
                DimensionManager.entityLoop(world, entity, ticks, deltaTime);
                BlockHandler.entityLoop(entity);

                if (entity instanceof LivingEntity)
                    CustomItemManager.entityLoop((LivingEntity) entity, ticks);
            }
    }

    /**
     * Gets the current instance of this plugin.
     *
     * @return The current instance of this plugin.
     */
    public static PaperThinger getInstance() {
        return instance;
    }

    // TODO Give players all recipes they don't know when they join.
    /*@EventHandler
    public static void recipeGiver(PlayerJoinEvent playerJoinEvent) {
        Player player = playerJoinEvent.getPlayer();

        while (recipes.hasNext())
            player.discoverRecipe(recipes.next());
    }*/

    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (label.equalsIgnoreCase("bind"))
            return ItemHandler.onCommandBind(sender, command, label, args);

        else if (label.equalsIgnoreCase("cgive"))
            return CustomItemManager.onCommandCGive(sender, command, label, args);

        else if (label.equalsIgnoreCase("recursion"))
            return NitwitBits.onCommandRecursion(sender, command, label, args);

        // Reverses the gravitational state of entities
        else if (label.equalsIgnoreCase("toggleGravity")) {
            if (args.length < 1) {
                sender.sendMessage(Color.RED + "Not enough arguments.");
                return false;
            }

            List<Entity> entities = MiscPaperFunctions.hijackEntitiesFromSelector((Entity) sender, args[0]);

            if (entities.size() == 0) {
                sender.sendMessage(Color.RED + "No entities found.");
            }

            for (Entity entity : entities)
                entity.setGravity(!entity.hasGravity());
        }

        return true;
    }
}
