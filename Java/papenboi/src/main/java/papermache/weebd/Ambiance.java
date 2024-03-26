package papermache.weebd;

import org.bukkit.Material;
import org.bukkit.Particle;
import org.bukkit.block.Block;
import org.bukkit.block.data.Waterlogged;
import org.bukkit.entity.Entity;

import java.util.Vector;

/**
 *  Adds in various effects to make the world look cooler and more alive.
 */
final class Ambiance {
    private static Vector<Entity> burningBois = new Vector<>();

    // Extinguishing particles
    static void entityPeriodic(Entity ent) {
        Block b = ent.getLocation().getBlock();

        if (ent.getFireTicks() < 0 && burningBois.contains(ent) && (b.getType() == Material.WATER || (b.getBlockData() instanceof Waterlogged &&
                ((Waterlogged) b.getBlockData()).isWaterlogged()))) {
            ent.getWorld().spawnParticle(Particle.CLOUD, ent.getLocation().add(0, 0.75, 0), 15, 0, 0.5, 0, 0.08);
            burningBois.remove(ent);
        } else if (ent.getFireTicks() > 0 && !burningBois.contains(ent))
            burningBois.add(ent);
    }
}
