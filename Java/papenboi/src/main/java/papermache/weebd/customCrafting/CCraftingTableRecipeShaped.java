package papermache.weebd.customCrafting;

import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.Recipe;
import org.jetbrains.annotations.NotNull;

public class CCraftingTableRecipeShaped implements Recipe {
    private ItemStack result;
    private ItemStack[] ingredients = new ItemStack[9];
    private boolean positionSensitve, useMeta;

    /**
     *  Empty custom crafting table recipe constructor for continuity's sake.
     */
    public CCraftingTableRecipeShaped() {
        result = new ItemStack(Material.AIR);
        for (int i = 0; i < 9; i++)
            ingredients[i] = null;
        positionSensitve = false;
        useMeta = false;
    }

    /**
     *  Constructor for a custom crafting table recipe.
     *  Ingredients array must be true for: ingredients.length == 9.
     *
     * @param ingredients The array containing the recipe of an item, use null for empty slots.
     * @param result The item to craft from the ingredients.
     * @param usesMeta Whether or not the recipe is meta sensitive.
     * @param isPositionSensitive Whether or not the location of the recipe in the table matters.
     *
     * @throws ArrayIndexOutOfBoundsException if ingredients.length is not 9.
     */
    public CCraftingTableRecipeShaped(@NotNull ItemStack[] ingredients, @NotNull ItemStack result, boolean usesMeta, boolean isPositionSensitive) {
        if (ingredients.length != 9)
            throw new ArrayIndexOutOfBoundsException("A recipe matrix must have 9 slots!");

        this.result = result;
        this.ingredients = ingredients.clone();
        positionSensitve = isPositionSensitive;
        useMeta = usesMeta;
    }

    /**
     *  Gets the resulting item of a recipe.
     *
     * @return The result of a recipe.
     */
    @Override
    @NotNull
    public ItemStack getResult() {
        return result;
    }

    /**
     *  Sets the resulting item of a recipe.
     *
     * @param result The new resulting item of a crafting recipe.
     */
    public void setResult(@NotNull ItemStack result) {
        this.result = result;
    }

    /**
     *  Gets the ingredients of a recipe.
     *  Note that the entry's of the array may be null.
     *
     * @return A 9 length array containing the ingredients of a recipe.
     */
    @NotNull
    public ItemStack[] getIngredients() {
        return ingredients;
    }

    /**
     *  Sets the ingredients of a recipe
     *  Note that the entry's of the array may be null.
     *
     * @param ingredients A 9 length array containing the new ingredients for the recipe.
     *
     * @throws ArrayIndexOutOfBoundsException if ingredients.length is not 9.
     */
    public void setIngredients(@NotNull ItemStack[] ingredients) {
        if (ingredients.length != 9)
            throw new ArrayIndexOutOfBoundsException("A recipe matrix must have 9 slots!");

        this.ingredients = ingredients.clone();
    }

    /**
     *  Tells whether or not a recipe is position sensitive (i.e. the position of the recipe in the crafting recipe matters.)
     *
     * @return Whether or not the recipe is position sensitive.
     */
    public boolean isPositionSensitve() {
        return positionSensitve;
    }

    /**
     *  Sets whether or not a recipe is position sensitive (i.e. the position of the recipe in the crafting recipe matters.)
     *
     * @param isPositionSensitive Sets whether or not the recipe is position sensitive.
     */
    public void setPositionSensitve(boolean isPositionSensitive) {
        positionSensitve = isPositionSensitive;
    }

    /**
     *  Tells whether or not a recipe is meta sensitive (i.e. the meta of the items in the recipe matters.)
     *
     * @return Whether or not the recipe is position sensitive.
     */
    public boolean usesMeta() {
        return useMeta;
    }

    /**
     *  Tells whether or not a recipe is meta sensitive (i.e. the meta of the items in the recipe matters.)
     *
     * @param useMeta Sets whether or not the recipe is position sensitive.
     */
    public void setMetaUse(boolean useMeta) {
        this.useMeta = usesMeta();
    }
}
