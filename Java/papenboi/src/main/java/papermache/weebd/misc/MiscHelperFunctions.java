package papermache.weebd.misc;

import org.bukkit.*;
import org.bukkit.attribute.Attribute;
import org.bukkit.block.Block;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerItemBreakEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.Damageable;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.potion.PotionEffect;
import org.bukkit.util.Vector;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

/**
 *  A bunch of random helper functions, quite cute aren't they?
 */
public final class MiscHelperFunctions {
    /**
     *  Tests whether an entity is directly looking at another, using higher precision the farther the distance.
     *  Does not account for obstacles.
     *
     * @param chad The observer.
     * @param sal The observed.
     * @return true, if sal is being observed by chad.
     */
    public static boolean isLookingAt(Entity chad, Entity sal) {
        Vector chadLook = chad.getLocation().getDirection();
        Vector chad2Sal = sal.getLocation().toVector().subtract(chad.getLocation().toVector()).normalize();
        double distBetween = chad.getLocation().distanceSquared(sal.getLocation());
        double precision = 0.9;

        // Logarithmically increasing precision for exponentially long distances
        if (distBetween > 10) {
            int scalar = 100;

            int i = 1;
            while (!(Math.pow(10, i) > distBetween)) {
                precision += (9.0 / scalar);
                scalar *= 10;

                i++;
            }
        }

        return chadLook.dot(chad2Sal) > precision;
    }

    /**
     *  Gets the neighboring blocks of a location.
     *
     * @param loc The location to scan blocks for.
     * @return The neighboring blocks.
     */
    public static Map<Direction, Block> getNeighboringBlocks(Location loc) {
        Map<Direction, Block> neighbors = new HashMap<>();

        neighbors.put(Direction.UP, loc.getWorld().getBlockAt(loc.getBlockX(), loc.getBlockY() + 1, loc.getBlockZ()));
        neighbors.put(Direction.DOWN, loc.getWorld().getBlockAt(loc.getBlockX(), loc.getBlockY() - 1, loc.getBlockZ()));
        neighbors.put(Direction.NORTH, loc.getWorld().getBlockAt(loc.getBlockX(), loc.getBlockY(), loc.getBlockZ() - 1));
        neighbors.put(Direction.SOUTH, loc.getWorld().getBlockAt(loc.getBlockX(), loc.getBlockY(), loc.getBlockZ() + 1));
        neighbors.put(Direction.WEST, loc.getWorld().getBlockAt(loc.getBlockX() - 1, loc.getBlockY(), loc.getBlockZ()));
        neighbors.put(Direction.EAST, loc.getWorld().getBlockAt(loc.getBlockX() + 1, loc.getBlockY(), loc.getBlockZ()));

        return neighbors;
    }

    /**
     *  Tries to apply a potion effect if: 1, the entity doesn't already have the effect. B, the amplifer is stronger than the current effect. Or 3, the amplifer is the same as,
     *  but the duration is longer than the current effect.
     *
     * @param e The entity to apply the effect to.
     * @param pe The effect to apply to the entity.
     * @return true, if successful.
     */
    public static boolean tryApplyPotionEffect(LivingEntity e, PotionEffect pe) {
        boolean successful = false;

        if (e.hasPotionEffect(pe.getType())) {
            PotionEffect current = e.getPotionEffect(pe.getType());
            int amp = Math.abs(current.getAmplifier());

            if (amp < Math.abs(pe.getAmplifier()))
                successful = e.addPotionEffect(pe, true);
            else if (amp == Math.abs(pe.getAmplifier())) {
                int duration = current.getDuration();

                if (duration < pe.getDuration()) {
                    successful = e.addPotionEffect(pe, true);
                }
            }
        } else
            successful = e.addPotionEffect(pe, true);

        return successful;
    }

    /**
     *  Tries to apply tag if: 1, the entity has no tags. Or 2, the entity doesn't have the tag.
     *
     * @param e The entity to apply the tag to.
     * @param tag The tag to apply to the entity.
     * @return true, if successful.
     */
    public static boolean addTagSafely(Entity e, String tag) {
        boolean successful = false;

        if (e.getScoreboardTags().size() > 0) {
            if (!e.getScoreboardTags().contains(tag))
                successful = e.addScoreboardTag(tag);
        } else
            successful = e.addScoreboardTag(tag);

        return successful;
    }

    /**
     *  Tries to remove a tag if the entity has the tag.
     *
     * @param e The entity to remove the tag from.
     * @param tag The tag to remove from the entity.
     * @return true, if successful.
     */
    public static boolean removeTagSafely(Entity e, String tag) {
        boolean successful = false;

        if (e.getScoreboardTags().size() > 0 && e.getScoreboardTags().contains(tag))
            successful = e.removeScoreboardTag(tag);

        return successful;
    }

    /**
     *  Sends a message to all players, optionally with the time as well.
     *  Can be sent with time of call, optionally, of course.
     *  The time in yyyy-MM-dd:hh-mm-ss.nnn format, where: y = year, M = month, d = day, h = hour, m = minute, s = second, and n = millisecond.
     *
     * @param msg The message to be sent.
     * @param includeTime Whether the time of execution should be included.
     */
    public static void announceMessage(String msg, boolean includeTime) {
        if (includeTime) {
            Bukkit.getServer().getOnlinePlayers().forEach(p -> p.sendMessage("[" + LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace('T',
                    ' ') + "] " + msg));
            Bukkit.getLogger().info("[" + LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).replace('T', ' ') + "] " + msg);
        } else {
            Bukkit.getServer().getOnlinePlayers().forEach(p -> p.sendMessage(msg));
            Bukkit.getLogger().info(msg);
        }
    }

    /**
     *  Gets a random index of an array with a given size.
     *
     * @param arrayLength The length of the array to derive an index from.
     * @return A random index of an array.
     * @throws NegativeArraySizeException if arrayLength is less than or equal to 0.
     */
    public static int randomIndex(int arrayLength) {
        if (arrayLength <= 0)
            throw new NegativeArraySizeException();

        return (int) Math.floor(Math.random() * arrayLength);
    }

    /**
     *  Kills a LivingEntity, even if it is a player in creative mode.
     *
     * @param ent The LivingEntity to kill.
     */
    public static void killLivingEntity(LivingEntity ent) {
        if (ent instanceof Player && ((Player) ent).getGameMode() == GameMode.CREATIVE)
            ent.setHealth(0);
        else
            ent.damage(ent.getHealth() * ent.getAttribute(Attribute.GENERIC_MAX_HEALTH).getValue());
    }

    /**
     *  Damages a tool, breaking it if there is no durability.
     *
     * @param player The owner of the tool breaking.
     * @param tool The tool to damage or break.
     * @param damage The amount of damage to do to the tool.
     * @param usesUnbreaking Whether or not unbreaking effects the durability of the item.
     *
     * @throws IllegalArgumentException If tool isn't a damageable item.
     */
    public static void damageTool(Player player, ItemStack tool, int damage, boolean usesUnbreaking) {
        if (!(tool.getItemMeta() instanceof Damageable))
            throw new IllegalArgumentException("Arg 'tool' of 'damageTool' must be damageable!");

        if (tool.getItemMeta().isUnbreakable())
            return;

        if (usesUnbreaking && tool.getItemMeta().hasEnchant(Enchantment.DURABILITY) && 100.0 - (100.0 / (tool.getItemMeta().getEnchantLevel(Enchantment.DURABILITY) + 1)) >= (Math.random() * 100))
            return;

        Damageable damageableMeta = (Damageable) tool.getItemMeta();

        damageableMeta.setDamage(damageableMeta.getDamage() + damage);

        if (damageableMeta.getDamage() > tool.getType().getMaxDurability()) {
            player.getWorld().spawnParticle(Particle.ITEM_CRACK, player.getEyeLocation().add(player.getLocation().getDirection()), 25, 0, 0, 0, 0.2, tool);
            player.getWorld().playSound(player.getLocation(), Sound.ENTITY_ITEM_BREAK, SoundCategory.PLAYERS, 1, 1);

            player.getInventory().remove(tool);
        } else
            tool.setItemMeta((ItemMeta) damageableMeta);
    }

    /**
     *  Creates a random polarity (+ or -).
     *
     * @return 1 or -1, at random.
     */
    public static int randomPolarity() {
        if (Math.random() <= .5)
            return 1;
        else
            return -1;
    }
}

