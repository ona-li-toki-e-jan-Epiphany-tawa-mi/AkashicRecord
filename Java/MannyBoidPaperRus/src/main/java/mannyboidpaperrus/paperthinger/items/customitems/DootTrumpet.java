package mannyboidpaperrus.paperthinger.items.customitems;

import mannyboidpaperrus.paperthinger.PaperThinger;
import mannyboidpaperrus.paperthinger.items.ItemHandler;
import org.bukkit.GameMode;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.player.PlayerShearEntityEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.Recipe;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.persistence.PersistentDataContainer;
import org.bukkit.persistence.PersistentDataType;
import org.bukkit.util.Vector;

// TODO add animation and sound for trumpet.
// TODO add some method to get the trumpet
// TODO make trumpet reverse the direction of projectiles.

public class DootTrumpet implements ICustomItem, Listener {
    private static final String name = "Trumpet";
    private static final String metaKey = "key_paperthinger_custommeta_" + name;

    @Override
    public String getItemName() {
        return name;
    }

    @Override
    public ItemStack getItem() {
        ItemStack dootTrumpet = new ItemStack(Material.SHEARS);
        ItemMeta dootTrumpetMeta = dootTrumpet.getItemMeta();
        PersistentDataContainer dootTrumpetCustomMeta = dootTrumpetMeta.getPersistentDataContainer();

        dootTrumpetMeta.setDisplayName(name);
        dootTrumpetMeta.setCustomModelData(1);
        dootTrumpetCustomMeta.set(new NamespacedKey(PaperThinger.getInstance(), metaKey), PersistentDataType.BYTE, (byte) 0);

        dootTrumpet.setItemMeta(dootTrumpetMeta);

        return dootTrumpet;
    }

    @Override
    public boolean usesStandardDurability() {
        return true;
    }

    @Override
    public Recipe getRecipe() {
        return null;
    }

    /**
     * Activates the great power of doot when it is called upon to smite the unworthy.
     */
    @EventHandler
    public static void onUseTrumpet(PlayerInteractEvent playerInteractEvent) {
        Action action = playerInteractEvent.getAction();
        ItemStack item = playerInteractEvent.getItem();

        if ((action.equals(Action.RIGHT_CLICK_BLOCK) || action.equals(Action.RIGHT_CLICK_AIR)) && item != null && item.getType().equals(Material.SHEARS)) {
            PersistentDataContainer itemCustomMeta = item.getItemMeta().getPersistentDataContainer();
            Player player = playerInteractEvent.getPlayer();

            if (itemCustomMeta.get(new NamespacedKey(PaperThinger.getInstance(), metaKey), PersistentDataType.BYTE) != null && !player.hasCooldown(Material.SHEARS)) {
                playerInteractEvent.setCancelled(true);

                if (!player.getGameMode().equals(GameMode.CREATIVE)) {
                    player.setCooldown(Material.SHEARS, 20);

                    if (playerInteractEvent.getHand() != null)
                        if (playerInteractEvent.getHand().equals(EquipmentSlot.HAND))
                            ItemHandler.damageItem(player, false, 1, true);
                        else
                            ItemHandler.damageItem(player, true, 1, true);
                }

                for (Entity entity : player.getWorld().getEntities())
                    if (!entity.getUniqueId().equals(player.getUniqueId())) {
                        double distance = entity.getLocation().distance(player.getLocation());

                        if (distance <= 15) {
                            Vector trumpetForce = entity.getLocation().toVector().subtract(player.getLocation().toVector());

                            trumpetForce.normalize();

                            if (distance <= 1)
                                trumpetForce.multiply(1.2);
                            else
                                trumpetForce.multiply(1.2 / Math.sqrt(distance));

                            trumpetForce.add(new Vector(0, 0.1, 0));

                            entity.setVelocity(entity.getVelocity().add(trumpetForce));
                        }
                    }
            }
        }
    }

    /**
     * Prevents players from shearing sheep with a trumpet. That'd be weird, no?
     */
    @EventHandler
    public static void preventPlayersShearingWithTrumpets(PlayerShearEntityEvent playerShearEntityEvent) {
        ItemStack heldItem = playerShearEntityEvent.getPlayer().getActiveItem();

        if (heldItem != null && heldItem.getType().equals(Material.SHEARS)) {
            PersistentDataContainer heldItemCustomMeta = heldItem.getItemMeta().getPersistentDataContainer();

            if (heldItemCustomMeta.has(new NamespacedKey(PaperThinger.getInstance(), metaKey), PersistentDataType.BYTE))
                playerShearEntityEvent.setCancelled(true);
        }
    }
}
