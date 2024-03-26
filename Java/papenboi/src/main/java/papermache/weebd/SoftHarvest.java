package papermache.weebd;

import org.bukkit.*;
import org.bukkit.block.Block;
import org.bukkit.block.data.Ageable;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.Damageable;
import org.bukkit.inventory.meta.ItemMeta;
import papermache.weebd.misc.MiscHelperFunctions;

/**
 *  Allows players to harvest plants without breaking them by using a right-click with a hoe.
 */
public final class SoftHarvest implements Listener {
    @EventHandler
    public void onClick(PlayerInteractEvent e) {
        Player p = e.getPlayer();
        Action click = e.getAction();
        ItemStack held = p.getInventory().getItemInMainHand();
        World w = p.getWorld();
        Block bb = e.getClickedBlock();

        if (click == Action.RIGHT_CLICK_BLOCK && (held.getType() == Material.WOODEN_HOE || held.getType() == Material.STONE_HOE || held.getType() == Material.GOLDEN_HOE ||
                held.getType() == Material.IRON_HOE || held.getType() == Material.DIAMOND_HOE) && bb != null) {
            boolean sendFeedback = false;

            if (bb.getType() == Material.WHEAT && ((Ageable) bb.getBlockData()).getMaximumAge() == ((Ageable) bb.getBlockData()).getAge()) {
                ItemStack seeds = new ItemStack(Material.WHEAT_SEEDS, (int) Math.floor(Math.random() * 4));

                if (seeds.getAmount() > 0) {
                    Ageable blockData = (Ageable) bb.getBlockData();
                    blockData.setAge(0);

                    bb.setBlockData(blockData);

                    seeds.setAmount(seeds.getAmount() - 1);
                } else
                    bb.setType(Material.AIR);

                if (seeds.getAmount() != 0)
                    w.dropItemNaturally(bb.getLocation(), seeds);
                w.dropItemNaturally(bb.getLocation(), new ItemStack(Material.WHEAT, 1));

                sendFeedback = true;
            }
            else if (bb.getType() == Material.CARROTS && ((Ageable) bb.getBlockData()).getMaximumAge() == ((Ageable) bb.getBlockData()).getAge()) {
                ItemStack carrots = new ItemStack(Material.CARROT, (int) Math.floor(Math.random() * 4));
                Ageable blockData = (Ageable) bb.getBlockData();
                blockData.setAge(0);

                bb.setBlockData(blockData);

                if (carrots.getAmount() > 0)
                    w.dropItemNaturally(bb.getLocation(), carrots);

                sendFeedback = true;
            }
            else if (bb.getType() == Material.POTATOES && ((Ageable) bb.getBlockData()).getMaximumAge() == ((Ageable) bb.getBlockData()).getAge()) {
                ItemStack potatoes = new ItemStack(Material.POTATO, (int) Math.floor(Math.random() * 4));
                Ageable blockData = (Ageable) bb.getBlockData();
                blockData.setAge(0);

                bb.setBlockData(blockData);

                if (potatoes.getAmount() > 0)
                    w.dropItemNaturally(bb.getLocation(), potatoes);
                if (Math.random() <= 0.02)
                    w.dropItemNaturally(bb.getLocation(), new ItemStack(Material.POISONOUS_POTATO));

                sendFeedback = true;
            }
            else if (bb.getType() == Material.BEETROOTS && ((Ageable) bb.getBlockData()).getMaximumAge() == ((Ageable) bb.getBlockData()).getAge()) {
                ItemStack seeds = new ItemStack(Material.BEETROOT_SEEDS, (int) Math.floor(Math.random() * 4));

                if (seeds.getAmount() > 0) {
                    Ageable blockData = (Ageable) bb.getBlockData();
                    blockData.setAge(0);

                    bb.setBlockData(blockData);

                    seeds.setAmount(seeds.getAmount() - 1);
                } else
                    bb.setType(Material.AIR);

                if (seeds.getAmount() > 0)
                    w.dropItemNaturally(bb.getLocation(), seeds);
                w.dropItemNaturally(bb.getLocation(), new ItemStack(Material.BEETROOT));

                sendFeedback = true;
            }
            else if (bb.getType() == Material.NETHER_WART && ((Ageable) bb.getBlockData()).getMaximumAge() == ((Ageable) bb.getBlockData()).getAge()) {
                ItemStack netherWart = new ItemStack(Material.NETHER_WART, (int) Math.ceil(Math.random() * 3));
                Ageable blockData = (Ageable) bb.getBlockData();
                blockData.setAge(0);

                bb.setBlockData(blockData);

                w.dropItemNaturally(bb.getLocation(), netherWart);

                sendFeedback = true;
            }
            else if (bb.getType() == Material.COCOA && ((Ageable) bb.getBlockData()).getMaximumAge() == ((Ageable) bb.getBlockData()).getAge()) {
                ItemStack cocoaBeans = new ItemStack(Material.COCOA_BEANS, (int) Math.ceil(Math.random() * 2));
                Ageable blockData = (Ageable) bb.getBlockData();
                blockData.setAge(0);

                bb.setBlockData(blockData);

                w.dropItemNaturally(bb.getLocation(), cocoaBeans);

                sendFeedback = true;
            }

            if (sendFeedback) {
                if (p.getGameMode() != GameMode.CREATIVE)
                    MiscHelperFunctions.damageTool(p, held, 1, true);

                p.spawnParticle(Particle.SWEEP_ATTACK, p.getEyeLocation().add(p.getLocation().getDirection().toLocation(p.getWorld())), 1);
                w.playSound(bb.getLocation(), Sound.ENTITY_SHEEP_SHEAR, SoundCategory.PLAYERS,1, (float) (0.5 - (Math.random() / 5)));
                p.updateInventory();
            }
        }
    }
}
