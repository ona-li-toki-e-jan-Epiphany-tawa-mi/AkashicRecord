package mannyboidpaperrus.paperthinger.worldlayers.voidd;

import com.sun.javafx.geom.Point2D;
import mannyboidpaperrus.paperthinger.PaperThinger;
import mannyboidpaperrus.paperthinger.items.ItemHandler;
import mannyboidpaperrus.paperthinger.items.LivingEntityArmorSlot;
import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperConstants;
import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperFunctions;
import mannyboidpaperrus.paperthinger.standardstuffs.SixAxisDirections;
import mannyboidpaperrus.paperthinger.worldlayers.DimensionManager;
import mannyboidpaperrus.paperthinger.worldlayers.IDimension;
import org.bukkit.*;
import org.bukkit.entity.*;
import org.bukkit.event.Listener;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.MapMeta;
import org.bukkit.map.MapCanvas;
import org.bukkit.map.MapPalette;
import org.bukkit.map.MapRenderer;
import org.bukkit.map.MapView;
import org.bukkit.persistence.PersistentDataContainer;
import org.bukkit.persistence.PersistentDataType;

import java.util.HashMap;
import java.util.UUID;
import java.util.Vector;

// Make time fluctuate, such as random ticks changing, length of ticks changing, etc... use void konstant.

// Rework a bunch of random tick events and what-not so that they invoke decay.
// Have void kick riders off of mounts occasionally.
// Decrease time it takes for items to despawn at lower levels.
// Have the void drag mobs into nearby holes. Make stronger as you get down.

// Players & Entities gain resistance to entropy by being healthy, well fed, and experienced.
// Add Entropy for entities and blocks.
// Make it so entropy gets worse as you go down.
// Add providence ring that takes damage in place of armor and tools and helps reduce entropy absorption.

// Possibly add random outcrops of matter every so often.
// Create tumorous, growing lumps of matter.

/**
 * The void, a weird place with weird things.
 * Time doesn't... flow normally.
 * Neither does space, as they are linked.
 * Maps fill with static.
 *
 * Beware of entropy.
 * Armor and Weapons decay.
 *
 * This is rock bottom.
 */
public class Void implements IDimension, Listener {
    private static final String name = "world_void";
    private static double dimensionalScalar = 1;
    private static double voidKonstant = 0; // Very much not constant.
    private static HashMap<UUID, Double> oldVelocities = new HashMap<>();

    @Override
    public void generateWorld() {
        DimensionManager.createWorld(name, new VoidGenerator(), World.Environment.THE_END,false);
        World the_void = Bukkit.getWorld(name);

        try {
            the_void.setGameRule(GameRule.COMMAND_BLOCK_OUTPUT, false);
            the_void.setGameRule(GameRule.DISABLE_RAIDS, true);
            the_void.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, false);
            the_void.setGameRule(GameRule.DO_FIRE_TICK, false);
            the_void.setGameRule(GameRule.DO_MOB_SPAWNING, false);
            the_void.setGameRule(GameRule.DO_WEATHER_CYCLE, false);
            the_void.setGameRule(GameRule.MOB_GRIEFING, false);
            the_void.setGameRule(GameRule.NATURAL_REGENERATION, false);
            the_void.setGameRule(GameRule.RANDOM_TICK_SPEED, 0);
            the_void.setGameRule(GameRule.SPECTATORS_GENERATE_CHUNKS, false);

        } catch (Exception ignored) {}
    }

    @Override
    public boolean exists() {
        return Bukkit.getWorld(name) != null;
    }

    @Override
    public World getWorld() {
        return Bukkit.getWorld(name);
    }

    @Override
    public double getScalar() {
        return dimensionalScalar;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public void onExit(Entity entity, SixAxisDirections direction) {
        oldVelocities.remove(entity.getUniqueId());
    }

    /**
     * Runs.
     */
    public static void loop(double timePassed, byte ticks) {
        Vector<Double> scalingFactor = DimensionManager.scalingFactor;
        Vector<World> dimensions = DimensionManager.dimensions;
        World theVoid = Bukkit.getWorld(name);

        if (theVoid != null) {
            voidKonstant = Math.cos(timePassed / 20);

            // Causes the scaling factor to change over time. It seems as though it's... breathing?
            dimensionalScalar = voidKonstant / 6 + 1;
            scalingFactor.set(dimensions.indexOf(theVoid), dimensionalScalar);

            if (ticks % 5 == 0)
                strikeLightning(theVoid);
        }
    }

    /**
     * Runs over entities.
     */
    public static void entityLoop(World world, Entity entity, byte ticks, double deltaTime) {
        World theVoid = Bukkit.getWorld(name);
        Location loc = entity.getLocation();

        if (world.equals(theVoid)) {
            if (entity instanceof LivingEntity) {
                LivingEntity livingEntity = (LivingEntity) entity;
                boolean creativeDamagePass = false;

                // Some particles and sound to spice up the decaying plane. TODO test the sound and make it cool.
                if (livingEntity.getType().equals(EntityType.PLAYER)) {
                    Player player = (Player) livingEntity;

                    if (ticks % 7 == 0) {
                        int particleCount;

                        if (loc.getY() >= 0) {
                            particleCount = (int) Math.ceil((theVoid.getMaxHeight() + 64 - loc.getY()) / 40 + voidKonstant);
                        } else
                            particleCount = (int) Math.round(8 + voidKonstant);

                        if (particleCount > 0)
                            player.spawnParticle(Particle.SPELL_WITCH, loc, particleCount, 6, 6, 6, 0);
                    }

                    if (Math.random() <= 1 / 9.0)
                        player.playSound(loc, Sound.ENTITY_WITHER_HURT, SoundCategory.AMBIENT, 0, 0.1F);

                    // Item decay ignores players in creative and spectator mode.
                    if (player.getGameMode().equals(GameMode.CREATIVE) || player.getGameMode().equals(GameMode.SPECTATOR)) {
                        creativeDamagePass = true;

                    } else if (ticks % 20 == 0)
                        if (player.getInventory().getItemInMainHand().getType().equals(Material.FILLED_MAP)) {
                            fillMapWithStatic(player.getInventory().getItemInMainHand(), theVoid);

                        } else if (player.getInventory().getItemInOffHand().getType().equals(Material.FILLED_MAP))
                            fillMapWithStatic(player.getInventory().getItemInOffHand(), theVoid);
                }

                // Decays horse armor.
                if (livingEntity.getType().equals(EntityType.HORSE) && (loc.getY() < 0 || Math.random() < Math.abs(voidKonstant / loc.getY() / 12))) {
                    ItemStack armor = ((Horse) entity).getInventory().getArmor();

                    if (armor != null) {
                        boolean contains = false;
                        for (Material horseArmur : MiscPaperConstants.horseArmor)
                            if (horseArmur.equals(armor.getType()))
                                contains = true;

                        if (contains) {
                            livingEntity.getWorld().spawnParticle(Particle.ITEM_CRACK, livingEntity.getEyeLocation().add(loc.getDirection()), 25, 0, 0, 0, 0.2, armor);
                            livingEntity.getWorld().playSound(loc, Sound.ENTITY_ITEM_BREAK, SoundCategory.PLAYERS, 1, 1);

                            ((Horse) livingEntity).getInventory().setArmor(null);
                        }
                    }
                }

                if (!ignoresVoid(livingEntity)) {
                    // Random item and armor decay.
                    if (livingEntity.getEquipment() != null && !creativeDamagePass && (loc.getY() <= 0 ||
                            Math.random() <= Math.abs(voidKonstant * 5 / loc.getY()))) {
                        double randomItemDecayTicket = Math.random();
                        int damage = 1;

                        if (loc.getY() < 0)
                            damage -= Math.floor(loc.getY() / 100);

                        if (randomItemDecayTicket <= 1 / 6.0 && livingEntity.getEquipment().getHelmet() != null)
                            ItemHandler.damageArmor(livingEntity, LivingEntityArmorSlot.HELMET, damage, true);

                        else if (randomItemDecayTicket <= 1 / 3.0 && livingEntity.getEquipment().getChestplate() != null)
                            ItemHandler.damageArmor(livingEntity, LivingEntityArmorSlot.CHESTPLATE, damage, true);

                        else if (randomItemDecayTicket <= 0.5 && livingEntity.getEquipment().getLeggings() != null)
                            ItemHandler.damageArmor(livingEntity, LivingEntityArmorSlot.LEGGINGS, damage, true);

                        else if (randomItemDecayTicket <= 2 / 3.0 && livingEntity.getEquipment().getBoots() != null)
                            ItemHandler.damageArmor(livingEntity, LivingEntityArmorSlot.BOOTS, damage, true);

                        else if (randomItemDecayTicket <= 5 / 6.0)
                            ItemHandler.damageItem(livingEntity, false, damage, true);

                        else
                            ItemHandler.damageItem(livingEntity, true, damage, true);
                    }
                }

            } else if (entity.getType().equals(EntityType.ITEM_FRAME)) {
                ItemStack heldItem = ((ItemFrame) entity).getItem();

                if (heldItem.getType().equals(Material.FILLED_MAP))
                    fillMapWithStatic(heldItem, theVoid);

                ((ItemFrame) entity).setItem(heldItem);
            }

            increaseGravity(entity, theVoid);
        }
    }

    /**
     * Fills player's maps with static whilst in the Void.
     *
     * @param map The map to curse.
     * @param theVoid An instance of the Void.
     */
    private static void fillMapWithStatic(ItemStack map, World theVoid) {
        MapMeta mapMeta = (MapMeta) map.getItemMeta();

        if (mapMeta.hasMapView() && mapMeta.getMapView() != null) {
            MapView mapView = mapMeta.getMapView();
            PersistentDataContainer mapCustomMeta = mapMeta.getPersistentDataContainer();
            String mapStaticKey = "key_" + name + "_map_fillstatic";
            NamespacedKey namespacedKey = new NamespacedKey(PaperThinger.getInstance(), mapStaticKey);

            if (!mapCustomMeta.has(namespacedKey, PersistentDataType.BYTE)) {
                mapView.addRenderer(new MapRenderer() {
                    @Override
                    public void render(MapView map, MapCanvas canvas, Player player) {
                        if (!player.getGameMode().equals(GameMode.CREATIVE) && !player.getGameMode().equals(GameMode.SPECTATOR) &&
                                player.getWorld().equals(theVoid)) {
                            double mapDistortionFactor;

                            if (player.getLocation().getY() > 0.25) {
                                mapDistortionFactor = 10 / player.getLocation().getY() + Math.abs(voidKonstant);

                            } else
                                mapDistortionFactor = 100 * (player.getLocation().getY() + 0.25) + 40 + voidKonstant;

                            for (int i = 0; i < mapDistortionFactor; i++) {
                                int x = (int) (Math.random() * 127);
                                int y = (int) (Math.random() * 127);
                                int colorValue = (int) (Math.random() * 255);

                                canvas.setPixel(x, y, MapPalette.matchColor(colorValue, colorValue, colorValue));
                            }
                        }
                    }
                });

                mapCustomMeta.set(namespacedKey, PersistentDataType.BYTE, (byte) 0);
                mapMeta.setMapView(mapView);
                map.setItemMeta(mapMeta);
            }
        }
    }

    /**
     * Increases the gravitational pull upon an entity.
     *
     * @param entity The entity to gravitate.
     * @param theVoid An instance of the Void.
     */
    private static void increaseGravity(Entity entity, World theVoid) {
        if (ignoresVoid(entity) || entity.getScoreboardTags().contains("noGravity"))
            return;

        if (entity.isOnGround()) {
            oldVelocities.put(entity.getUniqueId(), entity.getVelocity().getY());
            return;
        }

        if (entity instanceof LivingEntity) {
            if (((LivingEntity) entity).isGliding()) {
                oldVelocities.put(entity.getUniqueId(), entity.getVelocity().getY());
                return;
            }

            if (entity.getType().equals(EntityType.PLAYER) && ((Player) entity).isFlying()) {
                oldVelocities.put(entity.getUniqueId(), entity.getVelocity().getY());
                return;
            }
        }

        org.bukkit.util.Vector velocity = entity.getVelocity();
        double oldY;

        if (oldVelocities.containsKey(entity.getUniqueId())) {
            oldY = oldVelocities.get(entity.getUniqueId());

        } else
            oldY = velocity.getY();

        double gravitySpeed = ((theVoid.getMaxHeight() + 64 - entity.getLocation().getY() + voidKonstant) / 100);
        double newYVelocity = oldY - Math.abs((oldY - velocity.getY()) * gravitySpeed);

        oldVelocities.put(entity.getUniqueId(), velocity.getY());

        if (newYVelocity > 10) {
            velocity.setY(10);

        } else if (newYVelocity < -10) {
            velocity.setY(-10);

        } else
            velocity.setY(newYVelocity);

        entity.setVelocity(velocity);
    }

    // TODO scale lightning strike count to number of loaded chunks.
    /**
     * Strikes lightning randomly in loaded chunks.
     *
     * @param theVoid An instance of the void.
     */
    private static void strikeLightning(World theVoid) {
        Chunk strikedChunk = theVoid.getLoadedChunks() [
            (int) (theVoid.getLoadedChunks().length * Math.random())
        ];
        int randomX = (int) (16 * Math.random());
        int randomZ = (int) (16 * Math.random());

        Point2D strikePoint = MiscPaperFunctions.getPointFromChunkCoordinates(strikedChunk.getX(), strikedChunk.getZ(), randomX, randomZ);

        Location highestBlock = MiscPaperFunctions.getHighestBlockBelow(theVoid, (int) strikePoint.x, 255, (int) strikePoint.y, false);

        if (highestBlock != null) {
            theVoid.strikeLightning(new Location(theVoid, strikePoint.x, highestBlock.getY(), strikePoint.y));

        } else
            theVoid.strikeLightning(new Location(theVoid, strikePoint.x, 0, strikePoint.y));
    }

    /**
     * Gets whether or not entity is immune to the Void's effects.
     *
     * @param entity The entity to test.
     *
     * @return Whether or not entity is immune to the Void's effects.
     */
    private static boolean ignoresVoid(Entity entity) {
        switch (entity.getType()) {
            case WITHER_SKELETON:
            case WITHER:
            case CAT:
            case OCELOT:
            case ENDERMAN:
            case ENDER_DRAGON:
            case ENDERMITE:
                return true;

            case SHEEP:
                DyeColor sheepColor = ((Sheep) entity).getColor();

                return sheepColor != null && (sheepColor.equals(DyeColor.BLACK) || sheepColor.equals(DyeColor.PURPLE));
        }

        return false;
    }
}
