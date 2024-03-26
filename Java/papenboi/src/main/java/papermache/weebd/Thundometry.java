package papermache.weebd;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.weather.LightningStrikeEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.util.RayTraceResult;
import papermache.weebd.misc.MiscHelperFunctions;

/**
 *  Enhances lightning to have more of an impact on the game and allows players to harness it's power.
 */
// TODO add ball lightning
public final class Thundometry implements Listener {
    private static final double LIGHTNING_ROD_HEIGHT_DIFFERENCE = 25; // The vertical distance from a lightning strike at which it has a 100% chance to be redirected
    private static final double LIGHTNING_ROD_RANGE = 50; // The horizontal radius in-which a lightning strike can be rerouted to the rod

    private static final double STORMFIRE_AXE_RANGE = 100; // The max range at which the Stormfire axe can shoot lightning
    private static final double STORMFIRE_AXE_VERTICAL_THRESHOLD = 80; // The height a player has to be at to shoot lightning with the axe
    private static final int STORMFIRE_AXE_COOLDOWN_TIME = 200; // The time, in ticks, before a player can fire another lightning bolt

    // Stormfire Axe
    @EventHandler
    public void onPlayerInteract(PlayerInteractEvent e) {
        Player p = e.getPlayer();
        Action click = e.getAction();
        EquipmentSlot handIt2Ya = e.getHand();
        Location loc = p.getLocation();

        // Right click with main hand
        if (click == Action.RIGHT_CLICK_AIR && !p.isSneaking()) {
            ItemStack held;
            if (handIt2Ya == EquipmentSlot.HAND)
                held = p.getInventory().getItemInMainHand();
            else
                held = p.getInventory().getItemInOffHand();

            // Stormfire Axe (generic axes for now)
            if (loc.getY() >= STORMFIRE_AXE_VERTICAL_THRESHOLD && (held.getType() == Material.IRON_AXE || held.getType() == Material.DIAMOND_AXE || held.getType() == Material.GOLDEN_AXE)
                    && !p.hasCooldown(held.getType())) {
                boolean didStrike = false;

                for (Entity ent : loc.getWorld().getEntities()) {
                    if ((ent instanceof Player && p.getDisplayName().equals(((Player) ent).getDisplayName())) || loc.distance(ent.getLocation()) > STORMFIRE_AXE_RANGE)
                        continue;

                    if (p.hasLineOfSight(ent) && MiscHelperFunctions.isLookingAt(p, ent)) {
                        ent.getWorld().strikeLightning(ent.getLocation());

                        didStrike = true;
                        break;
                    }
                }

                if (!didStrike) {
                    RayTraceResult strikeInfo = p.rayTraceBlocks(STORMFIRE_AXE_RANGE);

                    if (strikeInfo != null && strikeInfo.getHitBlock() != null) {
                        strikeInfo.getHitBlock().getWorld().strikeLightning(strikeInfo.getHitBlock().getLocation());
                        didStrike = true;
                    }
                }

                if (didStrike) {
                    p.setCooldown(held.getType(), STORMFIRE_AXE_COOLDOWN_TIME);
                    p.updateInventory();
                }
            }
        }
    }

    // Lighting Rod
    @EventHandler
    public void onLightingStrike(LightningStrikeEvent e) {
        Entity thundi = e.getLightning();
        Location loc = thundi.getLocation();
        LightningStrikeEvent.Cause just = e.getCause();

        if (just == LightningStrikeEvent.Cause.WEATHER)
            for (Entity ent : thundi.getWorld().getEntities()) {
                if (ent.getUniqueId() == thundi.getUniqueId())
                    continue;

                Location loc2 = ent.getLocation();

                // Did you know that lighting rods don't actually attract lightning?, wacky.
                if (ent.getType() == EntityType.ARMOR_STAND && ent.getCustomName() != null && ent.getCustomName().equals("lightningRod") && loc2.getY() >= loc.getY()
                    && Math.sqrt(Math.pow(loc2.getX() - loc.getX(), 2) + Math.pow(loc2.getZ() - loc.getZ(), 2)) <= LIGHTNING_ROD_RANGE) {
                    double chance = (loc2.getY() - loc.getY()) / LIGHTNING_ROD_HEIGHT_DIFFERENCE;

                    if (Math.random() <= chance) {
                        // Reroutes lightning to the rod
                        e.setCancelled(true);
                        loc.getWorld().strikeLightning(loc2);

                        break;
                    }
                }
            }
    }
}
