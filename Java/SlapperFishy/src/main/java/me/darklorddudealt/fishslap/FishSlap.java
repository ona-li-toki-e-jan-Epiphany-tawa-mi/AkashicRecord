package me.darklorddudealt.fishslap;

import com.comphenix.protocol.PacketType;
import com.comphenix.protocol.ProtocolLibrary;
import com.comphenix.protocol.ProtocolManager;
import com.comphenix.protocol.events.ListenerPriority;
import com.comphenix.protocol.events.PacketAdapter;
import com.comphenix.protocol.events.PacketContainer;
import com.comphenix.protocol.events.PacketEvent;
import org.bukkit.*;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.potion.PotionEffect;
import org.bukkit.potion.PotionEffectType;
import org.bukkit.scheduler.BukkitRunnable;
import org.bukkit.util.Vector;

public final class FishSlap extends JavaPlugin implements Listener {
    private static final Material[] slapper = {Material.SALMON, Material.TROPICAL_FISH, Material.COD}; // Used for testing player-held items
    private static boolean stopTnt; // Used to dictate whether explosion sounds should be cancelled

    @Override
    public void onEnable() {
        // Some nice startup messages
        getLogger().info("Convincing chinese fish markets...");
        getLogger().info("Oiling fish...");
        getLogger().info("Making things... fishy...");
        getLogger().info("blop...");

        getServer().getPluginManager().registerEvents(this, this); // Registers listener to be used for onEntityDamage()
        ProtocolManager pm = ProtocolLibrary.getProtocolManager();

        // Runs when any sound packet is made on sever
        pm.addPacketListener(new PacketAdapter(this, ListenerPriority.NORMAL, PacketType.Play.Server.NAMED_SOUND_EFFECT) {
            @Override
            public void onPacketSending(PacketEvent event) {
                PacketContainer packet = event.getPacket(); // Makes code easier to read
                String soundName = packet.getSoundEffects().read(0).toString(); // Grabs the name of the intercepted sound

                if (stopTnt && soundName.contains("ENTITY_GENERIC_EXPLODE")) {
                    event.setCancelled(true); // Stops 'ENTITY_GENERIC_EXPLODE' from being sent to player clients
                }
            }
        });

        getLogger().info("FishSlap Initialized!"); // A nice startup message
    }

    @EventHandler
    public void onEntityDamage(EntityDamageByEntityEvent e) {
        if (e.getDamager() instanceof Player) {
            Player sal = null;
            Entity sal2 = null;
            boolean b; // Used to denote if the hit 'entity' is actually an entity or a player

            Player chad = (Player) e.getDamager();
            if (e.getEntity() instanceof Player) {
                sal = (Player) e.getEntity();
                b = true;
            } else {
                sal2 = e.getEntity();
                b = false;
            }

            if (chad != null) { // Safety first!
                Material held = chad.getInventory().getItemInMainHand().getType();
                if (held != null) // Safety first!
                    for (Material material : slapper) // Compares held item with the list of raw fish
                        if (held == material) {
                            double dist; // Denotes integer distance between the attacker and the attacked
                            Location loc; // Helps find position for explosion and animation

                            // This spaghetti tastes really nice

                            if (b) {
                                sal.addPotionEffect((new PotionEffect(PotionEffectType.DAMAGE_RESISTANCE, 2, 4))); // Prevents explosion from causing any damage
                                dist = Math.ceil(Math.sqrt(Math.pow((sal.getLocation().getX() - chad.getLocation().getX()), 2) + Math.pow((sal.getLocation().getY() - chad.getLocation().getY()), 2) + Math.pow((sal.getLocation().getZ() - chad.getLocation().getZ()), 2))); // ⌈√((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)⌉, defines how many iterations the floor loop should run off of
                                loc = sal.getLocation();
                                for (int x = 0; x < dist; x++) { // Approximates the point near but not equal to sal's coordinates, in the direction of chad
                                    //Midpoint madness
                                    loc.setX((loc.getX() + sal.getLocation().getX()) / 2);
                                    loc.setY((loc.getY() + sal.getLocation().getY()) / 2);
                                    loc.setZ((loc.getZ() + sal.getLocation().getZ()) / 2);
                                }
                                loc.setY(loc.getY() + 1); // Corrects animation and explosion to be more accurate to the chad's hit
                                stopTnt = true; // Stops following sound packet
                                sal.getWorld().createExplosion(loc, 0.25f); // Kinda for looks, kinda not
                                sal.getWorld().spawnParticle(Particle.CRIT, loc, 25, 0.125, 0.125, 0.125, 0.5); // Pleasing
                                // Thought it would be cool if it played a sound that corresponds to the fish used
                                if (held == Material.SALMON)
                                    sal.getWorld().playSound(loc, Sound.ENTITY_SALMON_HURT, SoundCategory.PLAYERS, 2, 0.1f);
                                else if (held == Material.COD)
                                    sal.getWorld().playSound(loc, Sound.ENTITY_COD_HURT, SoundCategory.PLAYERS, 2, 0.1f);
                                else
                                    sal.getWorld().playSound(loc, Sound.ENTITY_TROPICAL_FISH_HURT, SoundCategory.PLAYERS, 2, 0.1f);
                            } else {
                                ((LivingEntity) sal2).addPotionEffect((new PotionEffect(PotionEffectType.DAMAGE_RESISTANCE, 2, 4))); // Prevents explosion from causing any damage
                                dist = Math.ceil(Math.sqrt(Math.pow((sal2.getLocation().getX() - chad.getLocation().getX()), 2) + Math.pow((sal2.getLocation().getY() - chad.getLocation().getY()), 2) + Math.pow((sal2.getLocation().getZ() - chad.getLocation().getZ()), 2))); // ⌈√((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)⌉, defines how many iterations the floor loop should run off of
                                loc = sal2.getLocation();
                                for (int x = 0; x < dist; x++) {  // Approximates the point near but not equal to sal's coordinates, in the direction of chad
                                    //Midpoint madness
                                    loc.setX((loc.getX() + sal2.getLocation().getX()) / 2);
                                    loc.setY((loc.getY() + sal2.getLocation().getY()) / 2);
                                    loc.setZ((loc.getZ() + sal2.getLocation().getZ()) / 2);
                                }
                                loc.setY(loc.getY() + 1); // Corrects animation and explosion to be more accurate to the chad's hit
                                stopTnt = true; // Stops following sound packet
                                sal2.getWorld().createExplosion(loc, 0.25f); // Kinda for looks, kinda not
                                sal2.getWorld().spawnParticle(Particle.CRIT, loc, 25, 0.125, 0.125, 0.125, 0.5); // Pleasing
                                // Thought it would be cool if it played a sound that corresponds to the fish used
                                if (held == Material.SALMON)
                                    sal2.getWorld().playSound(loc, Sound.ENTITY_SALMON_HURT, SoundCategory.PLAYERS, 2, 0.1f);
                                else if (held == Material.COD)
                                    sal2.getWorld().playSound(loc, Sound.ENTITY_COD_HURT, SoundCategory.PLAYERS, 2, 0.1f);
                                else
                                    sal2.getWorld().playSound(loc, Sound.ENTITY_TROPICAL_FISH_HURT, SoundCategory.PLAYERS, 2, 0.1f);
                            }

                            Player finalSal1 = sal;
                            Entity finalSal = sal2;
                            new BukkitRunnable() { // Waits a short while for the entity to get some speed
                                @Override
                                public void run() {
                                    Vector vekky = b ? finalSal1.getVelocity() : finalSal.getVelocity(); // Grabs from correct entity variable
                                    // Really puts that... 'UMPH' into the slap
                                    vekky.setX(vekky.getX() * 5);
                                    vekky.setY(vekky.getY() * 2.5);
                                    vekky.setZ(vekky.getZ() * 5);
                                    if (b)
                                        finalSal1.setVelocity(vekky);
                                    else
                                        finalSal.setVelocity(vekky);
                                    stopTnt = false; // stops cancelling explosion sounds
                                }
                            }.runTaskLater(this, 1);
                        }
            }
        }
    }

        @Override
        public void onDisable () {
            // Some nice shutdown messages
            getLogger().info("Capin, the ship is sinking!");
            getLogger().info("glugluglugluglugluglugluglug...");
            getLogger().info("FishSlap is swimming with the fishies.");
        }

}


