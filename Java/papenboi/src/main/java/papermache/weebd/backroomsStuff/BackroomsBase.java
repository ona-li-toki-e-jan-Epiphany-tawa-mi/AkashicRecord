package papermache.weebd.backroomsStuff;

import org.bukkit.*;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.block.BlockPlaceEvent;
import org.bukkit.event.entity.EntitySpawnEvent;
import papermache.weebd.Weebd;
import papermache.weebd.misc.MiscHelperFunctions;

import java.util.Vector;

public final class BackroomsBase implements Listener {
    public static final String LEVEL_0_NAME = "backrooms_level_0"; // The name of the first level of the backrooms
    private static byte ticksPassed = 0;
    static Vector<int[]> chunkCoords = new Vector<>();

    public static void startup() {
        if (Bukkit.getWorld(LEVEL_0_NAME) == null) {
            WorldCreator wc = new WorldCreator(LEVEL_0_NAME);

            wc.generator(new Level0Generator());
            wc.generateStructures(false);

            wc.createWorld();

            if (Bukkit.getWorld(LEVEL_0_NAME) != null) {
                Bukkit.getWorld(LEVEL_0_NAME).setGameRule(GameRule.MOB_GRIEFING, false);
                Bukkit.getWorld(LEVEL_0_NAME).setGameRule(GameRule.DO_DAYLIGHT_CYCLE, false);
                Bukkit.getWorld(LEVEL_0_NAME).setGameRule(GameRule.DO_WEATHER_CYCLE, false);
                Bukkit.getWorld(LEVEL_0_NAME).setGameRule(GameRule.DO_FIRE_TICK, false);
            }
        }
    }

    // No breaking blocks in the backrooms, with the exception of creative players
    @EventHandler
    public void onBlockBreak(BlockBreakEvent e) {
        Player p = e.getPlayer();

        if (p.getWorld() == Weebd.level0 && p.getGameMode() != GameMode.CREATIVE)
            e.setCancelled(true);
    }

    // No placing blocks in the backrooms, with the exception of creative players
    @EventHandler
    public void onBlockPlace(BlockPlaceEvent e) {
        Player p = e.getPlayer();

        if (isBackrooms(p.getWorld()) && p.getGameMode() != GameMode.CREATIVE)
            e.setCancelled(true);
    }

    public static void periodic() {
        if (ticksPassed % 5 == 0)
            for (Entity ent : Weebd.level0.getEntities())
                if (ent instanceof Player)
                    ((Player) ent).setCompassTarget(new Location(ent.getWorld(), (int) Math.floor(MiscHelperFunctions.randomPolarity() * Math.random() * 60000000.0),
                            127, (int) Math.floor(MiscHelperFunctions.randomPolarity() * Math.random() * 60000000.0)));

        Weebd.level0.setTime(Weebd.level0.getTime() - 36);

        if (ticksPassed >= 127)
            ticksPassed = 0;
        else
            ticksPassed++;
    }

    // Prevents mob spawns
    @EventHandler
    public void onEntitySpawn(EntitySpawnEvent e) {
        Entity ent = e.getEntity();

        if (ent instanceof LivingEntity || ent.getType() != EntityType.ARMOR_STAND)
            e.setCancelled(true);
    }

    /**
     *  Gets whether or not world is a level of the backrooms.
     *
     * @param world The world to check.
     * @return Whether or not world is a level of the backrooms.
     */
    private boolean isBackrooms(World world) {
        return world == Weebd.level0;
    }
}
