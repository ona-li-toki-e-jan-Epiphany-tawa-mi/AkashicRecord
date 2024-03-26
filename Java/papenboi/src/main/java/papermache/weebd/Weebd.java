package papermache.weebd;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.*;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.entity.EntityPortalExitEvent;
import org.bukkit.event.entity.EntitySpawnEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerRespawnEvent;
import org.bukkit.event.player.PlayerTeleportEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitRunnable;
import org.bukkit.scoreboard.Scoreboard;
import org.jetbrains.annotations.NotNull;
import papermache.weebd.backroomsStuff.BackroomsBase;
import papermache.weebd.customCrafting.CraftingTableHandler;
import papermache.weebd.misc.MiscHelperFunctions;

/**
 *  The classic main class: boring, easy, full of simple checks; the epitome of the working man's code.
 */
// TODO add endervators (elevators)
// TODO add enderbeds that teleport you to a random enderbed when you wake up
// TODO make a backrooms world, possibly add command to send them there or when they say [REDACTED]
// TODO add rainbow leather armor
// TODO Hookshot
// TODO Space-Time Stonk Exchange wand
// TODO add bovine plane that kicks you out every five minutes, saying "recalibrating".
// TODO have sans appear in the leader-board
// TODO fix bug with totems of dying cancelling totems of undying on players
// TODO get custom crafting working with item meta
public final class Weebd extends JavaPlugin implements Listener {
    static Weebd currInstance;
    static World overworld, nether, end;
    public static World level0;

    // TODO add configuration files for this.
    private static final boolean CONVIENIENT_CRAFTS_ENABLED = true;
    private static final boolean DIMENSIONAL_WELCOME_MATS_ENABLED = true;
    private static final boolean FAST_LEAF_DECAY_ENABLED = true;
    private static final boolean HEAD_HUNTER_ENABLED = true;
    private static final boolean MOB_MASHUP_ENABLED = true;
    private static final boolean REDACTED_ENABLED = true;
    private static final boolean THUNDOMETRY_ENABLED = true;
    private static final boolean SOFT_HARVEST_ENABLED = true;
    private static final boolean FOREST_GODS_BLESSING_ENABLED = true;
    private static final boolean AMBIANCE_ENABLED = true;
    private static final boolean THE_BACKROOMS_ENABLED = true;

    @Override
    public void onEnable() {
        currInstance = this;

        overworld = getServer().getWorld("world");
        nether = getServer().getWorld("world_nether");
        end = getServer().getWorld("world_the_end");

        if (getServer().getScoreboardManager().getMainScoreboard().getObjective("isRightClicking") == null)
            getServer().getScoreboardManager().getMainScoreboard().registerNewObjective("isRightClicking", "dummy", "isRightClicking");

        getServer().getPluginManager().registerEvents(this, this);
        getServer().getPluginManager().registerEvents(new CraftingTableHandler(), this);
        if (HEAD_HUNTER_ENABLED)
            getServer().getPluginManager().registerEvents(new HeadHunter(), this);
        if (FAST_LEAF_DECAY_ENABLED)
            getServer().getPluginManager().registerEvents(new FastLeafDecay(), this);
        if (DIMENSIONAL_WELCOME_MATS_ENABLED)
            getServer().getPluginManager().registerEvents(new DWelcomeMat(), this);
        if (REDACTED_ENABLED)
            getServer().getPluginManager().registerEvents(new REDACTED(), this);
        if (THUNDOMETRY_ENABLED)
            getServer().getPluginManager().registerEvents(new Thundometry(), this);
        if (MOB_MASHUP_ENABLED)
            getServer().getPluginManager().registerEvents(new MobMashup(), this);
        if (SOFT_HARVEST_ENABLED)
            getServer().getPluginManager().registerEvents(new SoftHarvest(), this);

        if (FOREST_GODS_BLESSING_ENABLED) {
            ForestGodsBelssing.startup();
            getServer().getPluginManager().registerEvents(new ForestGodsBelssing(), this);
        }

        if (CONVIENIENT_CRAFTS_ENABLED)
            ConvenientCrafts.startup();
        if (Weebd.THE_BACKROOMS_ENABLED) {
            BackroomsBase.startup();
            getServer().getPluginManager().registerEvents(new BackroomsBase(), this);
            level0 = Bukkit.getWorld(BackroomsBase.LEVEL_0_NAME);
        }

        // Main loop
        new BukkitRunnable() { @Override public void run() { new BukkitRunnable() { @Override public void run() {
            Scoreboard mainScoreBoard = getServer().getScoreboardManager().getMainScoreboard();

            if (FAST_LEAF_DECAY_ENABLED)
                FastLeafDecay.periodic();
            if (THE_BACKROOMS_ENABLED)
                BackroomsBase.periodic();

            // Main entity loop
            for (World w : getServer().getWorlds())
                for (Entity ae : w.getEntities()) {
                    if (AMBIANCE_ENABLED)
                        Ambiance.entityPeriodic(ae);

                    if (REDACTED_ENABLED && ae instanceof LivingEntity)
                        REDACTED.livingEntityPeriodic((LivingEntity) ae);

                    if (ae instanceof Player) {
                        // Right click hold detection phase 2
                        mainScoreBoard.getObjective("isRightClicking").getScore(ae.getName()).setScore(0);
                    }
                }

        }}.runTaskTimer(currInstance, 0, 1); }}.runTaskLater(this, 10);
    }

    @EventHandler
    public void onPlayerInteract(PlayerInteractEvent e) {
        Player p = e.getPlayer();
        Action click = e.getAction();
        EquipmentSlot handIt2Ya = e.getHand();

        // Right click with main hand
        if ((click == Action.RIGHT_CLICK_AIR || click == Action.RIGHT_CLICK_BLOCK) && handIt2Ya == EquipmentSlot.HAND) {
            Scoreboard mainScoreBoard = getServer().getScoreboardManager().getMainScoreboard();

            // Right click hold detection phase 1
            mainScoreBoard.getObjective("isRightClicking").getScore(p.getName()).setScore(1);
        }
    }

    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent e) {
        Player p = e.getPlayer();

        updateDimensionalIDTags(p, p.getLocation());
    }

    @EventHandler
    public void onEntitySpawn(EntitySpawnEvent e) {
        Entity lilDude = e.getEntity();

        updateDimensionalIDTags(lilDude, lilDude.getLocation());
    }

    @EventHandler
    public void onPlayerRespawn(PlayerRespawnEvent e) {
        Player p = e.getPlayer();
        Location loc = e.getRespawnLocation();

        new BukkitRunnable() { @Override public void run() {
            updateDimensionalIDTags(p, loc);
        }}.runTaskLater(this, 1);
    }

    @EventHandler
    public void onPortalEvent(EntityPortalExitEvent e) {
        Entity ent = e.getEntity();
        Location loc2 = e.getTo();

        if (loc2 != null) {
            updateDimensionalIDTags(ent, loc2);
        }
    }

    @EventHandler
    public void onPlayerPortalEvent(PlayerTeleportEvent e) {
        Player p = e.getPlayer();
        Location loc2 = e.getTo();

        if (e.getCause() == PlayerTeleportEvent.TeleportCause.NETHER_PORTAL || e.getCause() == PlayerTeleportEvent.TeleportCause.END_PORTAL)
            updateDimensionalIDTags(p, loc2);
    }

    @Override
    public void onDisable() {

    }


    /**
     *  Updates certain tags on an entity that tell which dimension loc is at.
     *
     * @param e Then entity to update tags for.
     * @param loc The location to test for dimensions with.
     * @return true, if successful.
     */
    private boolean updateDimensionalIDTags(Entity e, Location loc) {
        boolean successful = false;

        if (loc.getWorld() == overworld) {
            successful = MiscHelperFunctions.addTagSafely(e, "overworld") &&
                    MiscHelperFunctions.removeTagSafely(e, "nether") &&
                    MiscHelperFunctions.removeTagSafely(e, "end") &&
                    MiscHelperFunctions.removeTagSafely(e, "level0");
        } else if (loc.getWorld() == nether) {
            successful = MiscHelperFunctions.addTagSafely(e, "nether") &&
                    MiscHelperFunctions.removeTagSafely(e, "overworld") &&
                    MiscHelperFunctions.removeTagSafely(e, "end") &&
                    MiscHelperFunctions.removeTagSafely(e, "level0");
        } else if (loc.getWorld() == end) {
            successful = MiscHelperFunctions.addTagSafely(e, "end") &&
                    MiscHelperFunctions.removeTagSafely(e, "overworld") &&
                    MiscHelperFunctions.removeTagSafely(e, "nether") &&
                    MiscHelperFunctions.removeTagSafely(e, "level0");
        } else if (THE_BACKROOMS_ENABLED && loc.getWorld() == level0) {
            successful = MiscHelperFunctions.addTagSafely(e, "level0") &&
                    MiscHelperFunctions.removeTagSafely(e, "overworld") &&
                    MiscHelperFunctions.removeTagSafely(e, "nether") &&
                    MiscHelperFunctions.removeTagSafely(e, "end");
        }

        return successful;
    }

    @Override
    public boolean onCommand(@NotNull CommandSender sender, @NotNull Command command, @NotNull String label, @NotNull String[] args) {
        Player p = (Player) sender;
        Location loc = p.getLocation();

        if (command.getName().equalsIgnoreCase("shift")) {
            if (args.length != 1 || Bukkit.getWorld(args[0]) == null)
                return false;

            loc.setWorld(Bukkit.getWorld(args[0]));
            p.teleport(loc);
            p.sendMessage("[Server -> me] Enjoy your stay ;)");
        }

        return true;
    }
}
