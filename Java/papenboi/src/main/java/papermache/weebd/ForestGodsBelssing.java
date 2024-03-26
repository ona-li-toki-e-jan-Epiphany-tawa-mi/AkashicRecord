package papermache.weebd;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockPlaceEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.ShapelessRecipe;
import org.bukkit.inventory.meta.ItemMeta;

import java.util.Collections;

/**
 *  The spell that the dragon guy (from mahoutsukai no yome) used to find his master, but now in minecraft form.
 */
public final class ForestGodsBelssing implements Listener {
    static void startup() {
        ItemStack spawnFinder = new ItemStack(Material.SPRUCE_SAPLING);
        ItemMeta spawnFinderMeta = spawnFinder.getItemMeta();

        spawnFinderMeta.setDisplayName("Blessing Of The Forest God");
        spawnFinderMeta.setLore(Collections.singletonList(ChatColor.GRAY + "Right click to find whats missing..."));
        spawnFinder.setItemMeta(spawnFinderMeta);

        ShapelessRecipe spawnFinder1 = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "spawnFinder_poppy_key"), spawnFinder);
        spawnFinder1.addIngredient(1, Material.SPRUCE_SAPLING);
        spawnFinder1.addIngredient(1, Material.STRING);
        spawnFinder1.addIngredient(1, Material.POPPY);
        Bukkit.addRecipe(spawnFinder1);

        ShapelessRecipe spawnFinder2 = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "spawnFinder_tulip_key"), spawnFinder);
        spawnFinder2.addIngredient(1, Material.SPRUCE_SAPLING);
        spawnFinder2.addIngredient(1, Material.STRING);
        spawnFinder2.addIngredient(1, Material.RED_TULIP);
        Bukkit.addRecipe(spawnFinder2);
    }

    @EventHandler
    public void onBlockPlace(BlockPlaceEvent e) {
        Player p = e.getPlayer();
        ItemStack held;

        if (e.getHand() == EquipmentSlot.HAND)
            held = p.getInventory().getItemInMainHand();
        else
            held = p.getInventory().getItemInOffHand();

        if (held.getType() == Material.SPRUCE_SAPLING && held.hasItemMeta() && held.getItemMeta().hasLore() && held.getItemMeta().getLore().contains(ChatColor.GRAY + "Right click to find whats missing..."))
            e.setCancelled(true);
    }
}
