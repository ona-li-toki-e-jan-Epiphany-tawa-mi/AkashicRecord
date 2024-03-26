package papermache.weebd;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerRespawnEvent;
import org.bukkit.event.player.PlayerTeleportEvent;
import org.bukkit.potion.PotionEffect;
import org.bukkit.potion.PotionEffectType;
import org.bukkit.scheduler.BukkitRunnable;
import papermache.weebd.misc.MiscHelperFunctions;

public final class DWelcomeMat implements Listener {
    private static final int TITLE_FADE_IN_TIME = 40; // The time, in ticks, that the dimensional greetings take to fade in
    private static final int TITLE_STAY_TIME = 100; // The time, in ticks, that the dimensional greetings remain fully opaque
    private static final int TITLE_FADE_OUT_TIME = 30; // The time, in ticks,  that the dimensional greetings take to fade away

    private static final int NEWBIE_PROTECTION_TIME = 400; // The time, in ticks, that new players & players without beds are completely immune to all damage after spawning in

    // Sends the player greetings when they join, and drops them from the sky if they are new
    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent e) {
        Player p = e.getPlayer();

        displayDimensionalGreetings(p, p.getLocation());

        if (!p.hasPlayedBefore()) {
            forkKnifeBattleBusRomHack2EletricBoogaloo(p, p.getLocation());
        }
    }

    // Sends the player greetings when they respawn, and drops them from the sky if they have no bed
    @EventHandler
    public void onPlayerRespawn(PlayerRespawnEvent e) {
        Player p = e.getPlayer();
        Location loc = e.getRespawnLocation();

        new BukkitRunnable() { @Override public void run() {
            displayDimensionalGreetings(p, loc);

            if (p.getBedSpawnLocation() == null)
                forkKnifeBattleBusRomHack2EletricBoogaloo(p, loc);
        }}.runTaskLater(Weebd.currInstance, 1);
    }

    // Sends the player greetings when they go through portals
    @EventHandler
    public void onPlayerPortalEvent(PlayerTeleportEvent e) {
        Player p = e.getPlayer();
        Location loc2 = e.getTo();

        if (e.getCause() == PlayerTeleportEvent.TeleportCause.NETHER_PORTAL || e.getCause() == PlayerTeleportEvent.TeleportCause.END_PORTAL)
            displayDimensionalGreetings(p, loc2);
    }


    /**
     *  Displays the 'greetings' of a player's dimension to them.
     *
     * @param p Then player to send greeting to.
     * @param loc The location to test for dimensions with.
     */
    private void displayDimensionalGreetings(Player p, Location loc) {
        if (loc.getWorld() == Weebd.overworld)
            p.sendTitle(ChatColor.DARK_GREEN + "The Overworld", ChatColor.GREEN + "" + ChatColor.ITALIC + "Life's Sanctuary", TITLE_FADE_IN_TIME, TITLE_STAY_TIME,
                    TITLE_FADE_OUT_TIME);
        else if (loc.getWorld() == Weebd.nether)
            p.sendTitle(ChatColor.DARK_RED + "The Nether", ChatColor.GOLD + "" + ChatColor.ITALIC + "Visceral Hell", TITLE_FADE_IN_TIME, TITLE_STAY_TIME,
                    TITLE_FADE_OUT_TIME);
        else if (loc.getWorld() == Weebd.end)
            p.sendTitle(ChatColor.DARK_PURPLE + "The End", ChatColor.BLACK + "" + ChatColor.BOLD + "" + ChatColor.ITALIC + "" + ChatColor.MAGIC + "???",
                    TITLE_FADE_IN_TIME, TITLE_STAY_TIME, TITLE_FADE_OUT_TIME);
    }

    /**
     *  Yeah...
     *  we droppin, boiz.
     *
     * @param p The player to drop from the hypothetical battle bus at location loc.
     * @param loc The location of the hypothetical battle bus.
     */
    private void forkKnifeBattleBusRomHack2EletricBoogaloo(Player p, Location loc) {
        loc.setY(255);
        p.teleport(loc);
        if (p.isFlying())
            p.setFlying(false);

        MiscHelperFunctions.tryApplyPotionEffect(p, new PotionEffect(PotionEffectType.DAMAGE_RESISTANCE, NEWBIE_PROTECTION_TIME, 4, true, true));
    }
}
