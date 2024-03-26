package papermache.weebd;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Creeper;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.SkullMeta;
import org.bukkit.scheduler.BukkitRunnable;

/**
 *  Allows players to decapitate each-other.
 *  Charged creepers can also decapitate players.
 *
 *  Possibly will add head lopping for other mobs as well.
 *  Possibly will make it not a 100% chance to get the head when killed by a player.
 */
final public class HeadHunter implements Listener {
    // Drops heads upon death in valid conditions
    @EventHandler
    public void onEntityHurt(EntityDamageByEntityEvent e) {
        Entity chad = e.getDamager();
        Entity sal = e.getEntity();
        Location salLoc = sal.getLocation();

        new BukkitRunnable() { @Override public void run() {
            if(sal.isDead() && (chad instanceof Player || (chad instanceof Creeper && ((Creeper) chad).isPowered()))) {
                if (sal instanceof Player) {
                    ItemStack newbHead = new ItemStack(Material.PLAYER_HEAD, 1);
                    SkullMeta newbiHeadData = (SkullMeta) newbHead.getItemMeta();

                    newbiHeadData.setOwningPlayer((OfflinePlayer) sal);
                    newbHead.setItemMeta(newbiHeadData);

                    salLoc.getWorld().dropItemNaturally(salLoc, newbHead);
                }
                // TODO Possibly add mobs without head drops to have head drops, but make it the same chance.
            }
        }}.runTaskLater(Weebd.currInstance, 1);
    }
}
