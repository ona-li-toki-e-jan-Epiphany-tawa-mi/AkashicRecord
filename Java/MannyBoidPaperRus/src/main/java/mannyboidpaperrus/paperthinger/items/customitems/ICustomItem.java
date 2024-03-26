package mannyboidpaperrus.paperthinger.items.customitems;

import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.Recipe;

/**
 * Some repeated garbage used for custom items.
 */
public interface ICustomItem {
    /**
     * Gets the name of the custom item.
     *
     * @return The name of the custom item.
     */
    String getItemName();

    /**
     * Gets the custom item in ItemStack form.
     *
     * @return The item.
     */
    ItemStack getItem();

    /**
     * Gets whether or not the custom uses standard Minecraft durability.
     *
     * @return Whether or not the custom uses standard Minecraft durability.
     */
    boolean usesStandardDurability();

    /**
     * Gets the recipe of this item.
     * Can return null if no recipe exists.
     *
     * @return The recipe of this item, or null, if it doesn't exist.
     */
    Recipe getRecipe();
}
