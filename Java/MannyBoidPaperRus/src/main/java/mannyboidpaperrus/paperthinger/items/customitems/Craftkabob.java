package mannyboidpaperrus.paperthinger.items.customitems;

import mannyboidpaperrus.paperthinger.PaperThinger;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.Recipe;
import org.bukkit.inventory.ShapedRecipe;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.persistence.PersistentDataContainer;
import org.bukkit.persistence.PersistentDataType;

/**
 * Crafting table on a stick, a true delicacy!
 */
public class Craftkabob implements ICustomItem, Listener {
    private static final String name = "Craftkabob";
    private static final String recipeKey = "key_paperthinger_recipe_" + name;
    private static final String metaKey = "key_paperthinger_custommeta_" + name;

    @Override
    public String getItemName() {
        return name;
    }

    @Override
    public ItemStack getItem() {
        ItemStack craftkabob = new ItemStack(Material.OAK_BOAT);
        ItemMeta craftkabobMeta = craftkabob.getItemMeta();
        PersistentDataContainer craftkabobCustomMeta = craftkabobMeta.getPersistentDataContainer();

        craftkabobMeta.setDisplayName(name);
        craftkabobMeta.setCustomModelData(1);
        craftkabobCustomMeta.set(new NamespacedKey(PaperThinger.getInstance(), metaKey), PersistentDataType.BYTE, (byte) 0);

        craftkabob.setItemMeta(craftkabobMeta);

        return craftkabob;
    }

    @Override
    public boolean usesStandardDurability() {
        return false;
    }

    @Override
    public Recipe getRecipe() {
        ShapedRecipe craftkabobRecipe = new ShapedRecipe(new NamespacedKey(PaperThinger.getInstance(), recipeKey), getItem());

        craftkabobRecipe.shape("C ",
                               " S");

        craftkabobRecipe.setIngredient('C', Material.CRAFTING_TABLE);
        craftkabobRecipe.setIngredient('S', Material.STICK);

        return craftkabobRecipe;
    }

    /**
     * Opens a crafting table when a player uses a Craftkabob.
     */
    @EventHandler
    public static void onUseCraftkabob(PlayerInteractEvent playerInteractEvent) {
        Action action = playerInteractEvent.getAction();
        ItemStack item = playerInteractEvent.getItem();

        if ((action.equals(Action.RIGHT_CLICK_BLOCK) || action.equals(Action.RIGHT_CLICK_AIR)) && item != null && item.getType().equals(Material.OAK_BOAT)) {
                ItemMeta itemMeta = item.getItemMeta();
                PersistentDataContainer itemCustomMeta = itemMeta.getPersistentDataContainer();

                if (itemCustomMeta.has(new NamespacedKey(PaperThinger.getInstance(), metaKey), PersistentDataType.BYTE)) {
                    Player player = playerInteractEvent.getPlayer();

                    player.openWorkbench(player.getLocation(), true);
                    playerInteractEvent.setCancelled(true);
                }
            }
    }
}
