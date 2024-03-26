package lllllllllllllllll.abhorrentideas.items;

import lllllllllllllllll.abhorrentideas.AbhorrentIdeas;
import lllllllllllllllll.abhorrentideas.items.armor.BaseArmor;
import net.minecraft.init.SoundEvents;
import net.minecraft.inventory.EntityEquipmentSlot;
import net.minecraft.item.Item;
import net.minecraft.item.ItemArmor;
import net.minecraftforge.common.util.EnumHelper;

import java.util.Vector;

/**
 * Handles stuff related to items.
 */
public class ItemHandler {
    public static final Vector<Item> ITEMS = new Vector<>();

    ///////////////////////////////////////////
    // Dirt Armor that has a cactus texture. //
    //                                       //
    // Idea by: u/MCrafterzz                 //
    // On: r/shittymcsuggestions             //
    ///////////////////////////////////////////

    static final ItemArmor.ArmorMaterial ARMOR_MATERIAL_DIRT = EnumHelper.addArmorMaterial(
            "armor_material_dirt",
            AbhorrentIdeas.MOD_ID + ":cactus_armor_texture",
            28,
            new int[] {6, 12, 16, 6},
            20,
            SoundEvents.BLOCK_GRASS_BREAK,
            4
    );

    public static final Item DIRT_HELMET = new BaseArmor("dirt_helmet", ARMOR_MATERIAL_DIRT, 1, EntityEquipmentSlot.HEAD).setMaxDamage(1525252433);
    public static final Item DIRT_CHESTPLATE = new BaseArmor("dirt_chestplate", ARMOR_MATERIAL_DIRT, 1, EntityEquipmentSlot.CHEST).setMaxDamage(1525252433);
    public static final Item DIRT_LEGGINGS = new BaseArmor("dirt_leggings", ARMOR_MATERIAL_DIRT, 2, EntityEquipmentSlot.LEGS).setMaxDamage(1525252433);
    public static final Item DIRT_BOOTS = new BaseArmor("dirt_boots", ARMOR_MATERIAL_DIRT, 1, EntityEquipmentSlot.FEET).setMaxDamage(1525252433);

    ////////////////////
    // Dirt Armor END //
    ////////////////////
}
