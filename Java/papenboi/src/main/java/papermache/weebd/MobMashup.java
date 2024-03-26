package papermache.weebd;

import org.bukkit.entity.Animals;
import org.bukkit.entity.Entity;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntitySpawnEvent;

/**
 *  Adds some random things for mobs.
 */
final class MobMashup implements Listener {
    // Causes 1/3 of naturally generated mobs to spawn as babies
    @EventHandler
    public void onMobSpawn(EntitySpawnEvent e) {
        Entity ent = e.getEntity();

        if (ent instanceof Animals && ((Animals) ent).isAdult() && Math.random() <= 0.33)
            ((Animals) ent).setBaby();
    }
}
