package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.chanceofprecipitation.statuseffects;

import com.destroystokyo.paper.event.entity.EntityJumpEvent;
import com.destroystokyo.paper.event.player.PlayerJumpEvent;
import org.bukkit.*;
import org.bukkit.attribute.Attribute;
import org.bukkit.attribute.AttributeInstance;
import org.bukkit.attribute.AttributeModifier;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.event.entity.PlayerDeathEvent;
import org.bukkit.event.player.PlayerInteractAtEntityEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.player.PlayerToggleFlightEvent;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.capabilities.CapabilitiesCore;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.capabilities.Capability;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.capabilities.StatusEffectCapability;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers.AttributeHelper;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers.DeathMessageLists;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers.EntityAffectors;

import java.util.Set;

/**
 * The freeze effect from risk of rain 2.
 *
 * Freeze effect prevents movement, rotation, and actions made by entities.
 * Entities that are under 30% of their maximum health are executed.
 */
public class FreezeEffect extends StatusEffectCapability implements Listener {
    private float initialEntityYaw;
    private boolean initialYawGiven;
    private float initialEntityPitch;
    private boolean initialPitchGiven;

    public FreezeEffect(String extraData) {
        super(extraData);

        String[] splitExtraData = extraData.split(",");

        if (splitExtraData.length >= 4) {
            try {
                initialEntityYaw = Float.parseFloat(splitExtraData[4]);
                initialYawGiven = true;

            } catch (NumberFormatException ignore) {
                initialEntityYaw = 0;
                initialYawGiven = false;
            }

        } else {
            initialEntityYaw = 0;
            initialYawGiven = false;
        }

        if (splitExtraData.length >= 5) {
            try {
                initialEntityPitch = Float.parseFloat(splitExtraData[5]);
                initialPitchGiven = true;

            } catch (NumberFormatException ignore) {
                initialEntityPitch = 0;
                initialPitchGiven = false;
            }

        } else {
            initialEntityPitch = 0;
            initialPitchGiven = false;
        }
    }

    @Override
    public Capability useConstructor(String extraData) {
        return new FreezeEffect(extraData);
    }

    @Override
    public String getCapabilityName() {
        return "COP:SE_freeze";
    }

    @Override
    public String getBaseName() {
        return "chance_of_precipretation.statuseffect.freeze.name";
    }

    @Override
    public String getExtraData() {
        return super.getExtraData() + "," + initialEntityYaw + "," + initialEntityPitch;
    }



    @Override
    public void runCapability(Entity entity) {
        super.runCapability(entity);

        // Prevents entities from rotating.
        Location entityLocation = entity.getLocation();

        if (entityLocation.getYaw() != initialEntityYaw || entityLocation.getPitch() != initialEntityPitch) {
            entityLocation.setYaw(initialEntityYaw);
            entityLocation.setPitch(initialEntityPitch);

            EntityAffectors.retainingTeleport(entity, entityLocation);
        }

        // Executes low-health entities.
        if (entity instanceof LivingEntity) {
            LivingEntity livingEntity = (LivingEntity) entity;
            boolean validEntity = true;
            boolean wasPlayer = false;

            if (livingEntity instanceof Player) {
                GameMode playerGameMode = ((Player) livingEntity).getGameMode();

                if (playerGameMode.equals(GameMode.CREATIVE) || playerGameMode.equals(GameMode.SPECTATOR)) {
                    validEntity = false;

                } else
                    wasPlayer = true;
            }

            if (validEntity) {
                AttributeInstance maxHealth = livingEntity.getAttribute(Attribute.GENERIC_MAX_HEALTH);

                if (maxHealth != null && livingEntity.getHealth() <= 0.3 * maxHealth.getValue()) {
                    if (wasPlayer)
                        livingEntity.addScoreboardTag("COP:SE_FE-PD");

                    livingEntity.setHealth(0);
                }
            }
        }
    }

    @Override
    public void onAssignment(Entity entity) {
        super.onAssignment(entity);

        if (entity instanceof LivingEntity) {
            LivingEntity livingEntity = (LivingEntity) entity;

            AttributeInstance movementSpeed = livingEntity.getAttribute(Attribute.GENERIC_MOVEMENT_SPEED);
            AttributeInstance flyingSpeed = livingEntity.getAttribute(Attribute.GENERIC_FLYING_SPEED);

            AttributeHelper.addModifiersSafely(movementSpeed, new AttributeModifier("COP:SE_FE-M2", -1, AttributeModifier.Operation.MULTIPLY_SCALAR_1));
            AttributeHelper.addModifiersSafely(flyingSpeed, new AttributeModifier("COP:SE_FE-M2", -1, AttributeModifier.Operation.MULTIPLY_SCALAR_1));

            // Knock em' outta the sky, boys.
            if (livingEntity instanceof Player) {
                Player player = (Player) livingEntity;

                if (player.isFlying())
                    player.setFlying(false);
            }
        }

        Location entityLocation = entity.getLocation();

        if (!initialYawGiven)
            initialEntityYaw = entityLocation.getYaw();
        if (!initialPitchGiven)
            initialEntityPitch = entityLocation.getPitch();
    }

    @Override
    public void onRevoke(Entity entity) {
        super.onRevoke(entity);

        if (entity instanceof LivingEntity) {
            LivingEntity livingEntity = (LivingEntity) entity;

            AttributeInstance movementSpeed = livingEntity.getAttribute(Attribute.GENERIC_MOVEMENT_SPEED);
            AttributeInstance flyingSpeed = livingEntity.getAttribute(Attribute.GENERIC_FLYING_SPEED);

            AttributeHelper.removeModifiers(movementSpeed, "COP:SE_FE-M2", true);
            AttributeHelper.removeModifiers(flyingSpeed, "COP:SE_FE-M2", true);
        }

        World world = entity.getWorld();
        Location entityLocation = entity.getLocation();

        double halfHeight = entity.getHeight() * 0.5;
        entityLocation.setY(entityLocation.getY() + halfHeight);

        world.spawnParticle(Particle.BLOCK_CRACK, entityLocation, 40, 0, halfHeight * 0.125, 0, 1, Material.ICE.createBlockData());
        world.playSound(entityLocation, Sound.BLOCK_GLASS_BREAK, SoundCategory.NEUTRAL, 1, 1);
    }



    // Custom death messages.
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public static void onPlayerDeath(PlayerDeathEvent playerDeathEvent) {
        Player player = playerDeathEvent.getEntity();

        if (player.getScoreboardTags().contains("COP:SE_FE-PD")) {
            playerDeathEvent.setDeathMessage(DeathMessageLists.buildRandomDeathMessage(DeathMessageLists.FREEZE_DEATH_MESSAGES, player.getDisplayName()));
            player.removeScoreboardTag("COP:SE_FE-PD");
        }
    }



    // The following 7 event handlers prevent actions taken by those frozen.
    @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
    public static void onEntityJump(EntityJumpEvent entityJumpEvent) {
        LivingEntity livingEntity = entityJumpEvent.getEntity();
        Set<Capability> entityCapabilities = CapabilitiesCore.getCapabilities(livingEntity);

        for (Capability capability : entityCapabilities)
            if (capability instanceof FreezeEffect) {
                FreezeEffect freezeEffect = (FreezeEffect) capability;
                freezeEffect.setDuration((int) (freezeEffect.getDuration() * 0.95));

                entityJumpEvent.setCancelled(true);

                break;
            }
    }

    @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
    public static void onPlayerJump(PlayerJumpEvent playerJumpEvent) {
        Player player = playerJumpEvent.getPlayer();
        Set<Capability> playerCapabilities = CapabilitiesCore.getCapabilities(player);

        for (Capability capability : playerCapabilities)
            if (capability instanceof FreezeEffect) {
                FreezeEffect freezeEffect = (FreezeEffect) capability;
                freezeEffect.setDuration(freezeEffect.getDuration() - 3);

                playerJumpEvent.setCancelled(true);

                break;
            }
    }

    @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
    public static void onEntityHitEntity(EntityDamageByEntityEvent entityDamageByEntityEvent) {
        Entity attacker = entityDamageByEntityEvent.getDamager();
        Set<Capability> entityCapabilities = CapabilitiesCore.getCapabilities(attacker);

        for (Capability capability : entityCapabilities)
            if (capability instanceof FreezeEffect) {
                entityDamageByEntityEvent.setCancelled(true);
                break;
            }
    }

    @EventHandler(priority = EventPriority.LOWEST)
    public static void onPlayerInteract(PlayerInteractEvent playerInteractEvent) {
        if (!playerInteractEvent.useInteractedBlock().equals(Event.Result.DENY) || !playerInteractEvent.useItemInHand().equals(Event.Result.DENY)) {
            Player player = playerInteractEvent.getPlayer();
            Set<Capability> playerCapabilities = CapabilitiesCore.getCapabilities(player);

            for (Capability capability : playerCapabilities)
                if (capability instanceof FreezeEffect) {
                    playerInteractEvent.setCancelled(true);
                    break;
                }
        }
    }

    @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
    public static void onPlayerInteractEntity(PlayerInteractEntityEvent playerInteractEntityEvent) {
        Player player = playerInteractEntityEvent.getPlayer();
        Set<Capability> playerCapabilities = CapabilitiesCore.getCapabilities(player);

        for (Capability capability : playerCapabilities)
            if (capability instanceof FreezeEffect) {
                playerInteractEntityEvent.setCancelled(true);
                break;
            }
    }

    @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
    public static void onPlayerInteractAtEntity(PlayerInteractAtEntityEvent playerInteractAtEntityEvent) {
        Player player = playerInteractAtEntityEvent.getPlayer();
        Set<Capability> playerCapabilities = CapabilitiesCore.getCapabilities(player);

        for (Capability capability : playerCapabilities)
            if (capability instanceof FreezeEffect) {
                playerInteractAtEntityEvent.setCancelled(true);
                break;
            }
    }

    @EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
    public static void onPlayerToggleFlight(PlayerToggleFlightEvent playerToggleFlightEvent) {
        if (playerToggleFlightEvent.isFlying()) {
            Player player = playerToggleFlightEvent.getPlayer();
            Set<Capability> playerCapabilities = CapabilitiesCore.getCapabilities(player);

            for (Capability capability : playerCapabilities)
                if (capability instanceof FreezeEffect) {
                    playerToggleFlightEvent.setCancelled(true);
                    break;
                }
        }
    }
}
