package mannyboidpaperrus.paperthinger.items.customitems;

import mannyboidpaperrus.paperthinger.potioneffects.PotionEffectHandler;
import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperFunctions;
import org.bukkit.*;
import org.bukkit.block.Block;
import org.bukkit.block.data.Levelled;
import org.bukkit.block.data.type.Farmland;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.PrepareItemCraftEvent;
import org.bukkit.inventory.CraftingInventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.Recipe;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.potion.PotionEffect;
import org.bukkit.potion.PotionEffectType;

/**
 * A neat little had for dem sad boiz.
 */
public class SadBoiHelmet implements ICustomItem, Listener {
    private static final String name = "Sad Boi Helm";

    @Override
    public String getItemName() {
        return name;
    }

    @Override
    public ItemStack getItem() {
        ItemStack sadHelm = new ItemStack(Material.LEATHER_HELMET);
        LeatherArmorMeta sadHelmMeta = (LeatherArmorMeta) sadHelm.getItemMeta();

        sadHelmMeta.setColor(Color.fromRGB(0xb2, 0xb8, 0xdf)); // Make with 5 white dye and 3 blue.
        sadHelmMeta.setDisplayName(ChatColor.DARK_AQUA + "" + ChatColor.ITALIC + name);

        sadHelm.setItemMeta(sadHelmMeta);
        return sadHelm;
    }

    @Override
    public boolean usesStandardDurability() {
        return true;
    }

    @Override
    public Recipe getRecipe() {
        return null;
    }

    /**
     * Runs over entities.
     *
     * @param livingEntity The entity currently being checked on the loop.
     * @param ticks The amount of ticks that have passed since the last second.
     */
    public static void entityLoop(LivingEntity livingEntity, byte ticks) {
        World world = livingEntity.getWorld();

        if (livingEntity.getEquipment() != null) {
            ItemStack helmet = livingEntity.getEquipment().getHelmet();

            if (helmet != null && helmet.getType().equals(Material.LEATHER_HELMET)) {
                LeatherArmorMeta helmetMeta = (LeatherArmorMeta) helmet.getItemMeta();

                if (helmetMeta.getColor().equals(Color.fromRGB(0xb2, 0xb8, 0xdf))) {
                    try {
                        // Can't use water if you're in the nether.
                        if ((Bukkit.getWorld("world_nether") == null || !world.equals(Bukkit.getWorld("world_nether")))
                                && !livingEntity.getLocation().add(0, 4, 0).getBlock().getType().equals(Material.LAVA)) {
                            Block cloudBlock = world.getBlockAt(livingEntity.getLocation().add(0, 4, 0));
                            // Cool particles.
                            if (!cloudBlock.getType().equals(Material.WATER)) {
                                world.spawnParticle(Particle.CLOUD, livingEntity.getLocation().add(0, 4, 0), 12, 0.75, 0.25, 0.75, 0);
                                world.spawnParticle(Particle.FALLING_WATER, livingEntity.getLocation().add(0, 3.75, 0), 1, 0.75, 0, 0.75, 0);

                            } else
                                world.spawnParticle(Particle.WATER_BUBBLE, livingEntity.getLocation().add(0, 4, 0), 4, 0.75, 0.25, 0.75, 0);

                            // Luck fo' fishin'.
                            if (ticks % 80 == 0)
                                PotionEffectHandler.tryApplyPotionEffect(livingEntity, new PotionEffect(PotionEffectType.LUCK, 100, 0, true, false, true));

                            // Block effects.
                            if (ticks % 3 == 0) {
                                Block nearestBlockBelow = world.getBlockAt(MiscPaperFunctions.getHighestBlockBelow(livingEntity.getLocation().add(0, 4, 0), true));

                                // Hydrating farmland.
                                if (nearestBlockBelow.getType().equals(Material.FARMLAND)) {
                                    Farmland farmland = ((Farmland) nearestBlockBelow.getBlockData());

                                    if (Math.random() <= 0.075 && farmland.getMoisture() + 1 <= farmland.getMaximumMoisture()) {
                                        farmland.setMoisture(farmland.getMoisture() + 1);

                                        nearestBlockBelow.setBlockData(farmland);
                                    }

                                    // Extinguishing fires.
                                } else if (world.getBlockAt(nearestBlockBelow.getLocation().add(0, 1, 0)).getType().equals(Material.FIRE)) {
                                    Block fire = world.getBlockAt(nearestBlockBelow.getLocation().add(0, 1, 0));

                                    if (!nearestBlockBelow.getType().equals(Material.NETHERRACK) && !nearestBlockBelow.getType().equals(Material.MAGMA_BLOCK) &&
                                            Math.random() <= 0.12) {
                                        fire.setType(Material.AIR);
                                        world.playSound(fire.getLocation(), Sound.BLOCK_FIRE_EXTINGUISH, SoundCategory.BLOCKS, 1, 1);
                                    }

                                    // Filling cauldrons.
                                } else if (nearestBlockBelow.getType().equals(Material.CAULDRON)) {
                                    Levelled cauldron = (Levelled) nearestBlockBelow.getBlockData();

                                    if (Math.random() <= 0.00003 && cauldron.getLevel() < cauldron.getMaximumLevel()) {
                                        cauldron.setLevel(cauldron.getLevel() + 1);
                                        nearestBlockBelow.setBlockData(cauldron);
                                    }
                                }
                            }

                            // Extinguishes mobs.
                            if (ticks % 15 == 0) {
                                for (Entity entity : world.getEntities())
                                    if (entity.getFireTicks() >= 1) {
                                        boolean isUnder = MiscPaperFunctions.isEntityExposedTo(entity, livingEntity.getLocation().add(0, 4, 0), true);

                                        if (isUnder)
                                            entity.setFireTicks(0);
                                    }
                            }
                        } else {
                            world.spawnParticle(Particle.SMOKE_NORMAL, livingEntity.getLocation().add(0, 4, 0), 16, 0.75, 0.25, 0.75, 0);

                            if (ticks % 3 == 0)
                                world.playSound(livingEntity.getLocation().add(0, 4, 0), Sound.BLOCK_LAVA_EXTINGUISH, SoundCategory.PLAYERS, 1, 1);
                        }
                    } catch (Exception ignored) {}
                }
            }
        }
    }

    /**
     * Sets the result of a leather helm mixed with 5 white and 3 blue dye to the Sad Boi Helmet.
     */
    @EventHandler
    public void onCraftSadBoiHelmet(PrepareItemCraftEvent prepareItemCraftEvent) {
        CraftingInventory craftingInventory = prepareItemCraftEvent.getInventory();
        ItemStack result = craftingInventory.getResult();

        if (result != null && result.getType().equals(Material.LEATHER_HELMET)) {
            LeatherArmorMeta leatherHelmetMeta = (LeatherArmorMeta) result.getItemMeta();

            if (leatherHelmetMeta.getColor().equals(Color.fromRGB(0xb2, 0xb8, 0xdf)))
                craftingInventory.setResult(this.getItem());
        }
    }
}
