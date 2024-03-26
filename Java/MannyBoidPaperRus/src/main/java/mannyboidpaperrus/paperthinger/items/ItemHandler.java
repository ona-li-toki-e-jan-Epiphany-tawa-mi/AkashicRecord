package mannyboidpaperrus.paperthinger.items;

import com.google.common.collect.ImmutableList;
import mannyboidpaperrus.paperthinger.PaperThinger;
import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperConstants;
import org.bukkit.*;
import org.bukkit.block.Block;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.entity.Projectile;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.entity.ProjectileHitEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;
import org.bukkit.inventory.meta.Damageable;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.persistence.PersistentDataContainer;
import org.bukkit.persistence.PersistentDataType;

/**
 * Helps with the handling of normal items.
 */
public class ItemHandler implements Listener {
    /**
     * Damages a item held by a LivingEntity.
     * Fails if the item was unbreakable.
     *
     * @param entity The entity with the item to be damaged.
     * @param isOffHand Whether or not the item to damage is in the offhand.
     * @param damage The amount of damage to place on the item.
     * @param usesUnbreaking Whether or not unbreaking enchantments should be taken account for.
     *
     * @return Whether or not the item was successfully damaged.
     */
    public static boolean damageItem(LivingEntity entity, boolean isOffHand, int damage, boolean usesUnbreaking) {
        ItemStack item;

        if (entity.getEquipment() == null)
            return false;

        if (!isOffHand)
            item = entity.getEquipment().getItemInMainHand();

        else
            item = entity.getEquipment().getItemInOffHand();

        if (item.getType().equals(Material.AIR))
            return false;

        if (!(item.getItemMeta() instanceof Damageable))
            return false;

        if (item.getItemMeta().isUnbreakable())
            return false;

        boolean contains = false;
        boolean isArmor = false;

        if (MiscPaperConstants.toolList.contains(item.getType()))
            contains = true;

         if (MiscPaperConstants.armorList.contains(item.getType())) {
             contains = true;
             isArmor = true;
         }

         if (!contains)
             return false;

        Damageable damageableMeta = (Damageable) item.getItemMeta();
        boolean broken = false;

        for (int x = 0; x < Math.abs(damage); x++) {
            if (!isArmor) {
                if (usesUnbreaking && item.getItemMeta().hasEnchant(Enchantment.DURABILITY) &&
                        100.0 / (item.getItemMeta().getEnchantLevel(Enchantment.DURABILITY) + 1) >= Math.random() * 100)
                    continue;
            } else
                if (usesUnbreaking && item.getItemMeta().hasEnchant(Enchantment.DURABILITY) &&
                    40.0 / (item.getItemMeta().getEnchantLevel(Enchantment.DURABILITY) + 1) + 60 >= Math.random() * 100)
                continue;

            damageableMeta.setDamage(damageableMeta.getDamage() + damage);

            if (damageableMeta.getDamage() > item.getType().getMaxDurability()) {
                broken = true;
                break;
            }
        }

        if (broken || damageableMeta.getDamage() > item.getType().getMaxDurability()) {
            entity.getWorld().spawnParticle(Particle.ITEM_CRACK, entity.getEyeLocation().add(entity.getLocation().getDirection()), 25, 0, 0, 0, 0.2, item);
            entity.getWorld().playSound(entity.getLocation(), Sound.ENTITY_ITEM_BREAK, SoundCategory.PLAYERS, 1, 1);

            if (!isOffHand)
                entity.getEquipment().setItemInMainHand(null);

            else
                entity.getEquipment().setItemInOffHand(null);

        } else {
            item.setItemMeta((ItemMeta) damageableMeta);

            if (!isOffHand)
                entity.getEquipment().setItemInMainHand(item);

            else
                entity.getEquipment().setItemInOffHand(item);
        }

        return true;
    }

    /**
     * Damages a piece of armor on a LivingEntity.
     * Fails if the piece of armor was unbreakable.
     *
     * @param entity The entity with the armor piece to be damaged.
     * @param armorSlot The slot with the armor piece to be damaged.
     * @param damage The amount of damage to place on the armor piece.
     * @param usesUnbreaking Whether or not unbreaking enchantments should be taken account for.
     *
     * @return Whether or not the armor piece was successfully damaged.
     */
    public static boolean damageArmor(LivingEntity entity, LivingEntityArmorSlot armorSlot, int damage, boolean usesUnbreaking) {
        ItemStack armorPiece;

        if (entity.getEquipment() == null)
            return false;

        if (armorSlot.equals(LivingEntityArmorSlot.HELMET))
            armorPiece = entity.getEquipment().getHelmet();

        else if (armorSlot.equals(LivingEntityArmorSlot.CHESTPLATE))
            armorPiece = entity.getEquipment().getChestplate();

        else if (armorSlot.equals(LivingEntityArmorSlot.LEGGINGS))
            armorPiece = entity.getEquipment().getLeggings();

        else
            armorPiece = entity.getEquipment().getBoots();

        if (armorPiece == null)
            return false;

        if (!(armorPiece.getItemMeta() instanceof Damageable))
            return false;

        if (armorPiece.getItemMeta().isUnbreakable())
            return false;

        if (!MiscPaperConstants.armorList.contains(armorPiece.getType()))
            return false;

        Damageable armorPieceMeta = (Damageable)armorPiece.getItemMeta();
        boolean broken = false;

        for (int x = 0; x < Math.abs(damage); x++) {
            if (usesUnbreaking && armorPiece.getItemMeta().hasEnchant(Enchantment.DURABILITY) &&
                    40.0 / (armorPiece.getItemMeta().getEnchantLevel(Enchantment.DURABILITY) + 1) + 60 >= Math.random() * 100)
                continue;

            armorPieceMeta.setDamage(armorPieceMeta.getDamage() + damage);

            if (armorPieceMeta.getDamage() > armorPiece.getType().getMaxDurability()) {
                broken = true;
                break;
            }
        }

        if (broken || armorPieceMeta.getDamage() > armorPiece.getType().getMaxDurability()) {
            entity.getWorld().spawnParticle(Particle.ITEM_CRACK, entity.getEyeLocation().add(entity.getLocation().getDirection()), 25, 0, 0, 0, 0.2, armorPiece);
            entity.getWorld().playSound(entity.getLocation(), Sound.ENTITY_ITEM_BREAK, SoundCategory.PLAYERS, 1, 1);

            if (armorSlot.equals(LivingEntityArmorSlot.HELMET))
                entity.getEquipment().setHelmet(null);

            else if (armorSlot.equals(LivingEntityArmorSlot.CHESTPLATE))
                entity.getEquipment().setChestplate(null);

            else if (armorSlot.equals(LivingEntityArmorSlot.LEGGINGS))
                entity.getEquipment().setLeggings(null);

            else
                entity.getEquipment().setBoots(null);

        } else {
            armorPiece.setItemMeta((ItemMeta) armorPieceMeta);

            if (armorSlot.equals(LivingEntityArmorSlot.HELMET))
                entity.getEquipment().setHelmet(armorPiece);

            else if (armorSlot.equals(LivingEntityArmorSlot.CHESTPLATE))
                entity.getEquipment().setChestplate(armorPiece);

            else if (armorSlot.equals(LivingEntityArmorSlot.LEGGINGS))
                entity.getEquipment().setLeggings(armorPiece);

            else
                entity.getEquipment().setBoots(armorPiece);
        }

        return true;
    }

    /**
     * Binds a command to a held debug stick, if the sender has one that is.
     */
    public static boolean onCommandBind(CommandSender sender, Command command, String label, String[] args) {
        if (!(sender instanceof Player))
            return false;

        if (args.length == 0)
            return false;

        ItemStack debugStick = null;
        PlayerInventory playerInventory = ((Player) sender).getInventory();

        if (playerInventory.getItemInMainHand().getType().equals(Material.DEBUG_STICK))
            debugStick = playerInventory.getItemInMainHand();
        else if (playerInventory.getItemInOffHand().getType().equals(Material.DEBUG_STICK))
            debugStick = playerInventory.getItemInOffHand();

        if (debugStick == null)
            return false;

        StringBuilder fullCommand = new StringBuilder();

        for (String arg : args)
            fullCommand.append(arg).append(" ");

        ItemMeta debugStickMeta = debugStick.getItemMeta();
        PersistentDataContainer debugStickCustomMeta = debugStickMeta.getPersistentDataContainer();

        debugStickMeta.setLore(ImmutableList.of(ChatColor.LIGHT_PURPLE + "Bound Command:", "    " + fullCommand.toString()));
        debugStickCustomMeta.set(new NamespacedKey(PaperThinger.getInstance(), "key_paperthinger_custommeta_command"), PersistentDataType.STRING,
                fullCommand.toString());

        debugStick.setItemMeta(debugStickMeta);

        return true;
    }

    /**
     * Runs a command that has been bound to a debug stick on right-click.
     */
    @EventHandler
    public static void executeBoundCommand(PlayerInteractEvent playerInteractEvent) {
        Action action = playerInteractEvent.getAction();
        ItemStack item = playerInteractEvent.getItem();

        if ((action.equals(Action.RIGHT_CLICK_BLOCK) || action.equals(Action.RIGHT_CLICK_AIR)) && item != null && item.getType().equals(Material.DEBUG_STICK)) {
            PersistentDataContainer itemCustomMeta = item.getItemMeta().getPersistentDataContainer();

            if (itemCustomMeta.has(new NamespacedKey(PaperThinger.getInstance(), "key_paperthinger_custommeta_command"), PersistentDataType.STRING)) {
                Player player = playerInteractEvent.getPlayer();
                String command = itemCustomMeta.get(new NamespacedKey(PaperThinger.getInstance(), "key_paperthinger_custommeta_command"), PersistentDataType.STRING);

                if (command != null) {
                    Bukkit.dispatchCommand(player, command);
                    playerInteractEvent.setCancelled(true);
                }
            }
        }
    }

    /**
     * Causes flaming arrows to set stuff on fire.
     * A little bit finicky.
     */
    @EventHandler
    public static void onArrowHit(ProjectileHitEvent projectileHitEvent) {
        Block block = projectileHitEvent.getHitBlock();
        Projectile arrow = projectileHitEvent.getEntity();

        if (block != null && (arrow.getType().equals(EntityType.ARROW) || arrow.getType().equals(EntityType.SPECTRAL_ARROW)))
            if (arrow.getFireTicks() >= 1 && block.getType().isFlammable()) {
                Block arrowLocation = arrow.getWorld().getBlockAt(arrow.getLocation());
                boolean isAir = false;

                for (Material emptyBlock : MiscPaperConstants.emptyBlocks)
                    if (emptyBlock.equals(arrowLocation.getType())) {
                        isAir = true;
                        break;
                    }

                if (isAir)
                    arrowLocation.setType(Material.FIRE, true);
            }
    }
}