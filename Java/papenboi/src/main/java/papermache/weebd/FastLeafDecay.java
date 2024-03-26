package papermache.weebd;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.data.type.Leaves;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.LeavesDecayEvent;
import org.bukkit.scheduler.BukkitRunnable;
import papermache.weebd.misc.Direction;
import papermache.weebd.misc.MiscHelperFunctions;

import java.util.Enumeration;
import java.util.Map;
import java.util.Vector;

/**
 *  A convenient class that greatly speeds up the decay of leaves.
 *  Does not effect player-placed leaves.
 *
 *  This shouldn't interfere with other plugins, but be sure to check.
 */
public final class FastLeafDecay implements Listener {
    private static Vector<Block> decayHash = new Vector<>();

    private static final long LEAF_DECAY_DELAY = 2; // The time delay before more leaves are selected for decay

    // Gets neighboring natural leaves that are valid for decay and adds them to a cache
    @EventHandler
    public void onLeaf(LeavesDecayEvent e) {
        Location loc = e.getBlock().getLocation();
        Map<Direction, Block> helloN = MiscHelperFunctions.getNeighboringBlocks(loc);

        new BukkitRunnable() { @Override public void run() {
            if (!e.isCancelled())
                for (Block bb : helloN.values())
                    if ((bb.getType() == Material.OAK_LEAVES || bb.getType() == Material.BIRCH_LEAVES || bb.getType() == Material.SPRUCE_LEAVES || bb.getType() == Material.JUNGLE_LEAVES ||
                            bb.getType() == Material.ACACIA_LEAVES || bb.getType() == Material.DARK_OAK_LEAVES) && !((Leaves) bb.getBlockData()).isPersistent() &&
                            ((Leaves) bb.getBlockData()).getDistance() > 6 && !decayHash.contains(bb))
                        // Adds the leaves to be called on a cache to avoid asynchronous errors
                        decayHash.add(bb);
        }}.runTaskLater(Weebd.currInstance, LEAF_DECAY_DELAY);
    }

    static void periodic() {
        // Decays leaves in the cache, and then removes them
        if (!decayHash.isEmpty()) {
            Enumeration<Block> leves = decayHash.elements();

            while (leves.hasMoreElements()) {
                Block leafi = leves.nextElement();

                // Light recursion to get more leaves
                Bukkit.getPluginManager().callEvent(new LeavesDecayEvent(leafi));

                // And then destroyed
                leafi.breakNaturally();

                decayHash.remove(leafi);
            }
        }
    }
}
