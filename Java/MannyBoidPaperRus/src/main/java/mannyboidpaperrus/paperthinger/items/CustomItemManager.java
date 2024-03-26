package mannyboidpaperrus.paperthinger.items;

import mannyboidpaperrus.paperthinger.PaperThinger;
import mannyboidpaperrus.paperthinger.items.customitems.Craftkabob;
import mannyboidpaperrus.paperthinger.items.customitems.DootTrumpet;
import mannyboidpaperrus.paperthinger.items.customitems.ICustomItem;
import mannyboidpaperrus.paperthinger.items.customitems.SadBoiHelmet;
import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperFunctions;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.HashMap;
import java.util.List;
import java.util.Vector;

// Unify all data containers for custom item types under one string container.

/**
 * Manages custom tools.
 */
public class CustomItemManager {
    private final static Vector<ICustomItem> customItems = new Vector<>();

    /**
     * Runs over entities.
     */
    public static void entityLoop(LivingEntity livingEntity, byte ticks) {
        SadBoiHelmet.entityLoop(livingEntity, ticks);
    }

    public static void startUp() {
        Craftkabob craftkabob = new Craftkabob();
        SadBoiHelmet sadBoiHelmet = new SadBoiHelmet();
        DootTrumpet dootTrumpet = new DootTrumpet();

        Bukkit.getPluginManager().registerEvents(sadBoiHelmet, PaperThinger.getInstance());
        Bukkit.getPluginManager().registerEvents(craftkabob, PaperThinger.getInstance());
        Bukkit.getPluginManager().registerEvents(dootTrumpet, PaperThinger.getInstance());

        customItems.add(craftkabob);
        customItems.add(sadBoiHelmet);
        customItems.add(dootTrumpet);

        for (ICustomItem customItem : customItems)
            if (customItem.getRecipe() != null)
                Bukkit.addRecipe(customItem.getRecipe());
    }

    // TODO Possibly add support for item meta.
    /**
     * Allows players to give themselves and others the custom items that have been implemented so far.
     */
    public static boolean onCommandCGive(CommandSender sender, Command command, String label, String[] args) {
        List<Entity> entities;
        ICustomItem customItem = null;
        boolean isCustomItem = false;
        int itemCount = 1;

        if (args.length < 2)
            return false;

        for (ICustomItem testCustomItem : customItems)
            if (args[1].equalsIgnoreCase(testCustomItem.getItemName().replace(' ', '_'))) {
                isCustomItem = true;
                customItem = testCustomItem;
            }

        if (!isCustomItem)
            return false;

        entities = MiscPaperFunctions.hijackEntitiesFromSelector((Entity) sender, args[0]);

        if (entities.size() == 0) {
            sender.sendMessage(ChatColor.RED + "No entity was found");
            return false;
        }

        try {
            if (args.length >= 3)
                itemCount = Integer.parseInt(args[2]);
        } catch (Exception ignored) {
            return false;
        }

        // Gives items
        for (Entity entity : entities)
            if (entity instanceof Player)
                for (int i = 0; i < itemCount; i++) {
                    HashMap<Integer, ItemStack> extraItems = ((Player) entity).getInventory().addItem(customItem.getItem());

                    if (extraItems.size() > 0)
                        for (ItemStack item : extraItems.values())
                            entity.getWorld().dropItemNaturally(entity.getLocation(), item);
                }

        return true;
    }
}
