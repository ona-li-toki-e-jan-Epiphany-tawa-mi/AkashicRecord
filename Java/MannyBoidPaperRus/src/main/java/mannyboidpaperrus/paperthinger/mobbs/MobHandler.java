package mannyboidpaperrus.paperthinger.mobbs;

import org.bukkit.Material;
import org.bukkit.Particle;
import org.bukkit.entity.*;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntityCombustEvent;
import org.bukkit.event.entity.EntityDamageByEntityEvent;

/**
 * Handles vanilla mobs.
 */
public class MobHandler implements Listener {
    /**
     * Causes creepers to ignite when set aflame.
     */
    @EventHandler
    public void igniteBurningCreepers(EntityCombustEvent entityCombustEvent) {
        Entity entity = entityCombustEvent.getEntity();

        if (entity.getType().equals(EntityType.CREEPER))
            ((Creeper) entity).ignite();
    }

    /**
     * Allows players to pet their pets when they shift-right click with an empty hand.
     */
    @EventHandler
    public void onPetPets(EntityDamageByEntityEvent entityDamageByEntityEvent) {
        Entity possiblePlayer = entityDamageByEntityEvent.getDamager();

        if (possiblePlayer.getType() == EntityType.PLAYER && ((Player) possiblePlayer).isSneaking() && ((Player) possiblePlayer).getInventory().getItemInMainHand().getType().equals(Material.AIR)) {
            Entity possiblePet = entityDamageByEntityEvent.getEntity();

            if (possiblePet.getType().equals(EntityType.WOLF)) {
                if (((Wolf) possiblePet).isTamed()) {
                    entityDamageByEntityEvent.setCancelled(true);
                    possiblePet.getWorld().spawnParticle(Particle.HEART, possiblePet.getLocation().add(0, 0.25, 0), 7, 0.25, 0.25, 0.25);
                }

            } else if (possiblePet.getType().equals(EntityType.CAT))
                if (((Cat) possiblePet).isTamed()) {
                    entityDamageByEntityEvent.setCancelled(true);
                    possiblePet.getWorld().spawnParticle(Particle.HEART, possiblePet.getLocation(), 5);
                }
        }
    }
}
