package papermache.weebd.misc;

import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import java.util.Arrays;

public final class CustomItems {
    public static ItemStack createCustomItem(CustomMaterial customMaterial, int amount) {
        switch (customMaterial) {
            case TOTEM_OF_DYING:
                ItemStack totemOfDying = new ItemStack(Material.TOTEM_OF_UNDYING, amount);
                ItemMeta todMeta = totemOfDying.getItemMeta();

                todMeta.setDisplayName("Totem of Dying");
                // Add texture
                todMeta.setLore(Arrays.asList(ChatColor.GOLD + "Cocken bal you like, MHHHMMMMM", ChatColor.GRAY + "" + ChatColor.BOLD + "   -The Bean Yoda Gang"));
                totemOfDying.setItemMeta(todMeta);

                return totemOfDying;
            default:
                return null;
        }
    }
}

