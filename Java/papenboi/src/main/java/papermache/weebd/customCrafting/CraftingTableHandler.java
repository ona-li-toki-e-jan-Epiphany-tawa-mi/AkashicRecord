package papermache.weebd.customCrafting;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.event.inventory.PrepareItemCraftEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;

import java.util.Vector;

public class CraftingTableHandler implements Listener {
    public static Vector<CCraftingTableRecipeShaped> recipeList = new Vector<>();

    @EventHandler
    public void onPrepareItemCraftEvent(PrepareItemCraftEvent e) {
        if (e == null)
            return;
    }

    @EventHandler
    public void onCraftItemEvent(InventoryClickEvent e) {
        if (e == null)
            return;

        Inventory inv = e.getClickedInventory();
        if (inv == null)
            return;

        InventoryType invType = inv.getType();
        int slot = e.getSlot();

        if (invType == InventoryType.WORKBENCH && slot == 0 && inv.getItem(slot) != null) {
            ItemStack[] temp = {inv.getItem(1), inv.getItem(2), inv.getItem(3), inv.getItem(4), inv.getItem(5), inv.getItem(6),
                    inv.getItem(7), inv.getItem(8), inv.getItem(9)};
        }
    }
}
