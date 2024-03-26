package lllllllllllllllll.abhorrentideas.items.armor;

import lllllllllllllllll.abhorrentideas.AbhorrentIdeas;
import lllllllllllllllll.abhorrentideas.items.ItemHandler;
import net.minecraft.inventory.EntityEquipmentSlot;
import net.minecraft.item.ItemArmor;

/**
 * A base class for armor items.
 */
public class BaseArmor extends ItemArmor {
    public BaseArmor(String name, ArmorMaterial materialIn, int renderIndexIn, EntityEquipmentSlot equipmentSlotIn) {
        super(materialIn, renderIndexIn, equipmentSlotIn);

        setTranslationKey(AbhorrentIdeas.MOD_ID + "." + name);
        setRegistryName(name);

        ItemHandler.ITEMS.add(this);
    }
}
