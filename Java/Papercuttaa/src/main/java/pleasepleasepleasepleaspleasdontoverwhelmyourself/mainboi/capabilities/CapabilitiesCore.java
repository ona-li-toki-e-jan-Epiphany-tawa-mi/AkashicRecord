package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.capabilities;

import com.destroystokyo.paper.event.entity.EntityAddToWorldEvent;
import com.destroystokyo.paper.event.entity.EntityRemoveFromWorldEvent;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.OfflinePlayer;
import org.bukkit.World;
import org.bukkit.command.*;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.PlayerDeathEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.scheduler.BukkitRunnable;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.MainBoi;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers.CommandHelper;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers.LocalizedMessages;

import java.util.*;
import java.util.logging.Logger;

// TODO Add /capabilities debug setInterval (collector|assimilator) to set the interval time of the collector and the assimilator.
//   Have the debug logger follow suit.
// TODO Have debugInterval, collectorRunInterval, assimilatorRunInterval and be loaded to and from a file on plugin shutdown and startup.
// TODO Have /capabilities assign be able to override extra data.

/**
 * The code used to manage capabilities.
 *
 * Capabilities are persistent data structures for entities.
 *
 * A capability must extend Capability.java, the base class, or one of its child classes.
 * A capability must properly implement useConstructor() and getCapabilityName().
 * A capability then must be registered with registerCapability().
 * Then you are free to assign and revoke that capability to your heart's content.
 */
public final class CapabilitiesCore implements Listener, CommandExecutor, TabCompleter {
    // Stores entities with capabilities.
    private static final HashMap<Entity, Set<Capability>> ENTITY_CAPABILITY_QUEUE = new HashMap<>();
    // Stores registered capabilities, allowing easy String -> Capability conversion.
    private static final HashMap<String, Capability> CAPABILITIES_REGISTRY = new HashMap<>();
    private static final long collectorRunInterval = 300;
    private static final long assimilatorRunInterval = 600;

    public static void onEnable() {
        MainBoi mainBoi = MainBoi.getInstance();
        CapabilitiesCore capabilitiesCore = new CapabilitiesCore();

        mainBoi.getServer().getPluginManager().registerEvents(capabilitiesCore, mainBoi);

        PluginCommand capabilitiesCommand = Objects.requireNonNull(mainBoi.getCommand("capabilities"));
        capabilitiesCommand.setExecutor(capabilitiesCore);
        capabilitiesCommand.setTabCompleter(capabilitiesCore);

        new BukkitRunnable() { @Override public void run() {
            runAssimilator();
        }}.runTaskTimer(MainBoi.getInstance(), 50, assimilatorRunInterval);

        new BukkitRunnable() { @Override public void run() {
            runCollector();
        }}.runTaskTimer(MainBoi.getInstance(), 51, collectorRunInterval);
    }



    /**
     * Runs through the Entity Queue, getting rid of entities with no capabilities, and fixes discrepancies with it and entity tags.
     */
    private static void runCollector() {
        Set<Entity> removalQueue = new HashSet<>();

        for (Entity entity : ENTITY_CAPABILITY_QUEUE.keySet()) {
            Set<Capability> entityCapabilities = getCapabilitiesFromTags(entity);

            if (entityCapabilities.isEmpty()) {
                removalQueue.add(entity);

            } else {
                Set<Capability> activeEntityCapabilities = getCapabilities(entity);

                // Removes capabilities that are not contained in the tags.
                for (Capability activeCapability : activeEntityCapabilities) {
                    String activeCapabilityName = activeCapability.getCapabilityName();
                    boolean hasCapability = false;

                    for (Capability capability : entityCapabilities)
                        if (activeCapabilityName.equals(capability.getCapabilityName())) {
                            hasCapability = true;
                            break;
                        }

                    if (!hasCapability)
                        revokeCapability(entity, activeCapability);
                }

                activeEntityCapabilities = getCapabilities(entity);

                // Loads unloaded capabilities, fixes discrepancies with extra data.
                for (Capability capability : entityCapabilities) {
                    boolean hasCapability = false;

                    for (Capability activeCapability : activeEntityCapabilities) {
                        String activeCapabilityName = capability.getCapabilityName();
                        String capabilityName = capability.getCapabilityName();

                        if (capabilityName.equals(activeCapabilityName)) {
                            hasCapability = true;

                            // Overrides the capability queue's extra data onto the tags.
                            if (!capability.getExtraData().equals(activeCapability.getExtraData()))
                                for (String tag : entity.getScoreboardTags())
                                    if (tag.contains(activeCapabilityName)) {
                                        entity.removeScoreboardTag(tag);
                                        entity.addScoreboardTag(joinNameAndExtra(activeCapabilityName, activeCapability.getExtraData()));

                                        break;
                                    }

                            break;
                        }
                    }

                    if (!hasCapability)
                        ENTITY_CAPABILITY_QUEUE.get(entity).add(capability);
                }
            }
        }

        for (Entity entity : removalQueue)
            ENTITY_CAPABILITY_QUEUE.remove(entity);
    }

    /**
     * Runs through all loaded entities, looking for those with capabilities that are not in the queue, so that it can add them.
     */
    private static void runAssimilator() {
        for (World world : Bukkit.getWorlds())
            for (Entity entity : world.getEntities())
                if (!ENTITY_CAPABILITY_QUEUE.containsKey(entity)) {
                    Set<Capability> entityCapabilities = getCapabilitiesFromTags(entity);

                    if (!entityCapabilities.isEmpty())
                        ENTITY_CAPABILITY_QUEUE.put(entity, entityCapabilities);
                }
    }

    /**
     * Runs the capabilities for the entities in the queue.
     */
    public static void tickCapabilities() {
        for (Map.Entry<Entity, Set<Capability>> entityQueueEntry : ENTITY_CAPABILITY_QUEUE.entrySet()) {
            Entity entity = entityQueueEntry.getKey();
            Set<Capability> entityCapabilities = entityQueueEntry.getValue();

            for (Capability entityCapability : entityCapabilities)
                entityCapability.runCapability(entity);
        }
    }



    /**
     * Registers capabilities to the capabilities system.
     * Capabilities must be registered to operate.
     */
    public static void registerCapability(Capability capability) throws DuplicateRegistryNameException {
        String capabilityName = capability.getCapabilityName();

        if (CAPABILITIES_REGISTRY.containsKey(capabilityName))
            throw new DuplicateRegistryNameException("Duplicate Capability Registry name: '" + capabilityName + "'");

        CAPABILITIES_REGISTRY.put(capabilityName, capability);
    }

    /**
     * Joins a capability name and its extra data.
     *
     * @param capabilityName The name of the capability.
     * @param extraData The extra data of the capability.
     *
     * @return The joined form.
     */
    private static String joinNameAndExtra(String capabilityName, String extraData) {
        return extraData.equals("") ? capabilityName : capabilityName + "-" + extraData;
    }



    /**
     * Assigns a capability to an entity.
     *
     * @param entity The entity to assign with the capability.
     * @param capability The capability to assign.
     *
     * @return If the capability was successfully assigned.
     */
    public static boolean assignCapability(Entity entity, Capability capability) throws UnsupportedOperationException {
        String capabilityName = capability.getCapabilityName();

        if (CAPABILITIES_REGISTRY.containsKey(capabilityName)) {
            boolean hasCapability = false;

            for (Capability possibleMatch : getCapabilities(entity))
                if (possibleMatch.getCapabilityName().equals(capabilityName)) {
                    hasCapability = true;
                    break;
                }

            if (!hasCapability) {
                if (!ENTITY_CAPABILITY_QUEUE.containsKey(entity))
                    ENTITY_CAPABILITY_QUEUE.put(entity, getCapabilitiesFromTags(entity));

                entity.addScoreboardTag(joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()));

                //if (entity instanceof Player && entity.isOp())
                //    entity.sendMessage("You have been assigned the capability: " + ChatColor.YELLOW + joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()) + ChatColor.WHITE + ".");

                ENTITY_CAPABILITY_QUEUE.get(entity).add(capability);
                capability.onAssignment(entity);

                return true;
            }

        } else
            throw new UnsupportedOperationException(capability.getCapabilityName() + " is not a registered capability. Capabilities must be registered before they can be assigned.");

        return false;
    }

    /**
     * Revokes a capability from an entity.
     *
     * @param entity The entity to revoke the capability from.
     * @param capability The capability to revoke.
     *
     * @return If the capability was successfully revoked.
     */
    public static boolean revokeCapability(Entity entity, Capability capability) throws UnsupportedOperationException {
        String capabilityName = capability.getCapabilityName();

        if (CAPABILITIES_REGISTRY.containsKey(capabilityName)) {
            Set<Capability> entityCapabilities = getCapabilities(entity);

            for (Capability possibleMatch : entityCapabilities)
                if (possibleMatch.getCapabilityName().equals(capabilityName)) {
                    capability.onRevoke(entity);

                    for (String tag : entity.getScoreboardTags())
                        if (tag.contains(capabilityName)) {
                            entity.removeScoreboardTag(tag);
                            break;
                        }

                    Set<Capability> trueEntityCapabilities = getCapabilitiesFromTags(entity);

                    if (trueEntityCapabilities.isEmpty()) {
                        ENTITY_CAPABILITY_QUEUE.remove(entity);

                    } else
                        ENTITY_CAPABILITY_QUEUE.get(entity).remove(capability);

                    //if (entity instanceof Player && entity.isOp())
                    //    entity.sendMessage("The capability, " + ChatColor.YELLOW + joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()) + ChatColor.WHITE + ", has been revoked from you.");

                    return true;
                }

        } else
            throw new UnsupportedOperationException(capability.getCapabilityName() + " is not a registered capability. Capabilities must be registered before they can be revoked.");

        return false;
    }

    /**
     * Gets the capabilities an entity has with its tags.
     *
     * @param entity The entity to get capabilities from.
     *
     * @return The capabilities an entity has.
     */
    public static Set<Capability> getCapabilitiesFromTags(Entity entity) {
        Set<String> entityTags = entity.getScoreboardTags();
        Set<Capability> entityCapabilities = new HashSet<>();

        // Looks for registered capabilities.
        for (String entityTag : entityTags) {
            Capability capability = getCapabilityFromTag(entityTag);

            if (capability != null)
                entityCapabilities.add(capability);
        }

        return entityCapabilities;
    }

    /**
     * Gets an instance of capability from a tag, adding in any extra data the tag has.
     *
     * @param entityTag The tag to get the capability from.
     *
     * @return The capability a tag has.
     */
    public static Capability getCapabilityFromTag(String entityTag) {
        String capabilityName;
        String extraData;

        // Extracts the capability name and any extra data from the tag.
        if (entityTag.contains("-")) {
            String[] splitTag = entityTag.split("-", 2);

            capabilityName = splitTag[0];
            extraData = splitTag[1];

        } else {
            capabilityName = entityTag;
            extraData = "";
        }

        Capability capability;

        // Creates a copy of the capability with the new data.
        if (CAPABILITIES_REGISTRY.containsKey(capabilityName)) {
            capability = CAPABILITIES_REGISTRY.get(capabilityName).useConstructor(extraData);

        } else
            capability = null;

        return capability;
    }

    /**
     * Gets the capabilities an entity has.
     * This methods grabs directly from the queue, instead of from the entity tags, and is thus faster.
     * Method may not be 100% accurate to tags.
     *
     * @param entity The entity to get capabilities from.
     *
     * @return The capabilities an entity has.
     */
    public static Set<Capability> getCapabilities(Entity entity) {
        return ENTITY_CAPABILITY_QUEUE.getOrDefault(entity, new HashSet<>());
    }



    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // The following 4 event handlers control the loading and unloading of entities to and from the entity queue. //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    @EventHandler
    public static void onPlayerJoin(PlayerJoinEvent playerJoinEvent) {
        Player player = playerJoinEvent.getPlayer();
        Set<Capability> playerCapabilities = getCapabilitiesFromTags(player);

        if (!playerCapabilities.isEmpty())
            ENTITY_CAPABILITY_QUEUE.put(player, playerCapabilities);
    }

    @EventHandler
    public static void onPlayerQuit(PlayerQuitEvent playerQuitEvent) {
        Player player = playerQuitEvent.getPlayer();

        // Overrides extra data onto tags before unload.
        if (ENTITY_CAPABILITY_QUEUE.containsKey(player)) {
            Set<Capability> playerCapabilities = getCapabilitiesFromTags(player);
            Set<Capability> activePlayerCapabilities = getCapabilities(player);

            for (Capability capability : playerCapabilities)
                for (Capability activeCapability : activePlayerCapabilities) {
                    String activeCapabilityName = capability.getCapabilityName();

                    if (capability.getCapabilityName().equals(activeCapabilityName)) {
                        String activeExtraData = activeCapability.getExtraData();

                        if (!capability.getExtraData().equals(activeExtraData))
                            for (String tag : player.getScoreboardTags())
                                if (tag.contains(activeCapabilityName)) {
                                    player.removeScoreboardTag(tag);
                                    player.addScoreboardTag(joinNameAndExtra(activeCapabilityName, activeExtraData));

                                    break;
                                }

                        break;
                    }
                }
        }

        ENTITY_CAPABILITY_QUEUE.remove(player);
    }

    @EventHandler
    public static void onEntityLoad(EntityAddToWorldEvent entityAddToWorldEvent) {
        Entity entity = entityAddToWorldEvent.getEntity();

        if (!(entity instanceof Player)) {
            Set<Capability> entityCapabilities = getCapabilitiesFromTags(entity);

            if (!entityCapabilities.isEmpty())
                ENTITY_CAPABILITY_QUEUE.put(entity, entityCapabilities);
        }
    }

    @EventHandler
    public static void onEntityUnload(EntityRemoveFromWorldEvent entityRemoveFromWorldEvent) {
        Entity entity = entityRemoveFromWorldEvent.getEntity();

        if (!(entity instanceof Player)) {
            // Overrides extra data onto tags before unload.
            if (!entity.isDead() && ENTITY_CAPABILITY_QUEUE.containsKey(entity)) {
                Set<Capability> entityCapabilities = getCapabilitiesFromTags(entity);
                Set<Capability> activeEntityCapabilities = getCapabilities(entity);

                for (Capability capability : entityCapabilities)
                    for (Capability activeCapability : activeEntityCapabilities) {
                        String activeCapabilityName = capability.getCapabilityName();

                        if (capability.getCapabilityName().equals(activeCapabilityName)) {
                            String activeExtraData = activeCapability.getExtraData();

                            if (!capability.getExtraData().equals(activeExtraData))
                                for (String tag : entity.getScoreboardTags())
                                    if (tag.contains(activeCapabilityName)) {
                                        entity.removeScoreboardTag(tag);
                                        entity.addScoreboardTag(joinNameAndExtra(activeCapabilityName, activeExtraData));

                                        break;
                                    }

                            break;
                        }
                    }
            }

            ENTITY_CAPABILITY_QUEUE.remove(entity);
        }
    }

    /**
     * Removes volatile capabilities on player death.
     */
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public static void onPlayerDeath(PlayerDeathEvent playerDeathEvent) {
        Player player = playerDeathEvent.getEntity();
        Set<Capability> playerCapabilities = CapabilitiesCore.getCapabilities(player);

        for (Capability capability : playerCapabilities)
            if (capability.isVolatile())
                CapabilitiesCore.revokeCapability(player, capability);
    }



    // A runnable used to periodically log debug information.
    private static BukkitRunnable debugRunnable = null;
    // The interval that the Entity Queue is dumped into the logs when debugging is on.
    private static long debugLoggerInterval = 1200;

    /**
     * Assigns and revokes registered capabilities through commands.
     */
    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (args.length >= 1)
            switch (args[0].toLowerCase()) {
                // /capabilities list...
                case "list":
                    // Lists all registered capabilities on the server.
                    if (args.length == 1) {
                        if (CAPABILITIES_REGISTRY.isEmpty()) {
                            sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.list.no_capabilities"));

                        } else {
                            List<String> messageList = new ArrayList<>();

                            messageList.add(LocalizedMessages.getMessageFor(sender, "command.capabilities.list.list_capabilities"));

                            for (String capabilityName : CAPABILITIES_REGISTRY.keySet())
                                messageList.add(" - " + ChatColor.YELLOW + capabilityName);

                            for (String message : messageList)
                                sender.sendMessage(message);
                        }

                    // Lists the capabilities of the entities found in the selector.
                    } else {
                        List<Entity> targets = CommandHelper.getCommandTargets(sender, args[1]);

                        if (targets.isEmpty()) {
                            sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command._common.entity.not_found")
                                    .replaceAll("%s", args[1]));

                        // Singular target.
                        } else if (targets.size() == 1) {
                            Entity target = targets.get(0);
                            Set<Capability> targetCapabilities = getCapabilities(target);

                            if (targetCapabilities.isEmpty()) {
                                sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.list.entity.no_capabilities")
                                        .replaceAll("%s", target.getName()));

                            } else {
                                List<String> messageList = new ArrayList<>();

                                messageList.add(LocalizedMessages.getMessageFor(sender, "command.capabilities.list.entity.list_capabilities")
                                        .replaceAll("%s", target.getName()));

                                for (Capability capability : targetCapabilities)
                                    messageList.add(" - " + ChatColor.GOLD + joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()));

                                for (String message : messageList)
                                    sender.sendMessage(message);
                            }

                        // Multiple targets.
                        } else {
                            Set<Capability> totalCapabilities = new HashSet<>();

                            for (Entity target : targets)
                                totalCapabilities.addAll(getCapabilities(target));

                            if (totalCapabilities.isEmpty()) {
                                sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.list.entities.no_capabilities")
                                        .replaceAll( "%s", Integer.toString(targets.size())));

                            } else {
                                List<String> messageList = new ArrayList<>();

                                messageList.add(LocalizedMessages.getMessageFor(sender, "command.capabilities.list.entities.list_capabilities")
                                        .replaceAll( "%s", Integer.toString(targets.size())));

                                for (Capability capability : totalCapabilities)
                                    messageList.add(" - " + ChatColor.GOLD + capability.getCapabilityName());

                                for (String message : messageList)
                                    sender.sendMessage(message);
                            }
                        }
                    }

                    return true;


                // Debug commands for testing and troubleshooting the capabilities system.
                case "debug":
                    if (args.length >= 2) {
                        switch (args[1].toLowerCase()) {
                            // Starts the debugger.
                            case "start":
                                if (debugRunnable == null) {
                                    debugRunnable = new BukkitRunnable() { @Override public void run() {
                                        logEntityQueueDump();
                                    }};
                                    debugRunnable.runTaskTimer(MainBoi.getInstance(), 100, debugLoggerInterval);

                                    sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.start.started")
                                            .replaceAll("%s", Long.toString(debugLoggerInterval)));

                                } else
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.start.already_started"));

                                break;

                            // Stops the debugger.
                            case "stop":
                                if (debugRunnable != null) {
                                    debugRunnable.cancel();
                                    debugRunnable = null;

                                    sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.stop.stopped"));

                                } else
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.stop.already_stopped"));

                                break;

                            // Prints out the entity queue.
                            case "dump":
                                logEntityQueueDump();
                                break;

                            // Sets the interval at which the entity queue is logged.
                            case "setinterval":
                                if (args.length >= 3) {
                                    try {
                                        debugLoggerInterval = Long.parseLong(args[2]);

                                        if (debugLoggerInterval > 0) {
                                            if (debugRunnable != null) {
                                                debugRunnable.cancel();

                                                debugRunnable = new BukkitRunnable() {
                                                    @Override
                                                    public void run() {
                                                        logEntityQueueDump();
                                                    }
                                                };
                                                debugRunnable.runTaskTimer(MainBoi.getInstance(), 100, debugLoggerInterval);
                                            }

                                            sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.setinterval.set")
                                                    .replaceAll("%s", Long.toString(debugLoggerInterval)));

                                        } else
                                            sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.setinterval.invalid_number")
                                                    .replaceAll("%s", args[2]));

                                    } catch (NumberFormatException ignored) {
                                        sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.setinterval.invalid_number")
                                                .replaceAll("%s", args[2]));
                                    }

                                } else
                                    sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.setinterval.usage"));

                                break;

                            default:
                                sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.usage"));
                        }

                    } else
                        sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.debug.usage"));

                    return true;


                // Assigns capabilities to entities.
                case "assign":
                    if (args.length >= 3) {
                        Capability capability = getCapabilityFromTag(args[2]);

                        if (capability != null) {
                            List<Entity> targets = CommandHelper.getCommandTargets(sender, args[1]);

                            if (targets.isEmpty()) {
                                sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command._common.entity.not_found")
                                        .replaceAll("%s", args[1]));

                            } else if (targets.size() == 1) {
                                Entity target = targets.get(0);
                                boolean success = assignCapability(target, capability);

                                if (success) {
                                    sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.assign.success")
                                            .replaceAll("%s1", ChatColor.YELLOW + joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()) + ChatColor.RESET)
                                            .replaceAll("%s2", target.getName()));

                                } else
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.assign.already_set")
                                            .replaceAll("%s", target.getName()));

                            } else {
                                int effectedEntityCount = 0;

                                for (Entity target : targets)
                                    if (assignCapability(target, capability)) {
                                        effectedEntityCount++;
                                    }

                                if (effectedEntityCount > 0) {
                                    sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.assign.success_multi")
                                            .replaceAll("%s1", ChatColor.YELLOW + joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()) + ChatColor.RESET)
                                            .replaceAll("%s2", Integer.toString(effectedEntityCount)));

                                } else
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.assign.already_set_multi"));
                            }

                        } else
                            sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.not_known")
                                    .replaceAll("%s", args[2]));

                    } else
                        sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.assign.usage"));

                    return true;


                // Revokes capabilities from entities.
                case "revoke":
                    if (args.length >= 3) {
                        // Revokes all capabilities from entities.
                        if (args[2].equals("__all")) {
                            List<Entity> targets = CommandHelper.getCommandTargets(sender, args[1]);

                            if (targets.isEmpty()) {
                                sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command._common.entity.not_found")
                                        .replaceAll("%s", args[1]));

                            } else if (targets.size() == 1) {
                                Entity target = targets.get(0);
                                Set<Capability> targetCapabilities = getCapabilities(target);

                                if (targetCapabilities.isEmpty()) {
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.has_none"));

                                } else {
                                    for (Capability capability : targetCapabilities)
                                        revokeCapability(target, capability);

                                    sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.removed_all")
                                            .replaceAll("%s", target.getName()));
                                }

                            } else {
                                boolean noCapabilities = true;
                                int effectedEntityCount = 0;

                                for (Entity target : targets) {
                                    Set<Capability> targetCapabilities = getCapabilities(target);

                                    if (!targetCapabilities.isEmpty()) {
                                        noCapabilities = false;
                                        effectedEntityCount++;

                                        for (Capability capability : targetCapabilities)
                                            revokeCapability(target, capability);
                                    }
                                }

                                if (noCapabilities) {
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.has_none_multi"));

                                } else
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.removed_all_multi")
                                            .replaceAll("%s", Integer.toString(effectedEntityCount)));
                            }

                        // Revokes a specific capability.
                        } else {
                            Capability capability = getCapabilityFromTag(args[2]);

                            if (capability != null) {
                                List<Entity> targets = CommandHelper.getCommandTargets(sender, args[1]);

                                if (targets.isEmpty()) {
                                    sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command._common.entity.not_found")
                                            .replaceAll("%s", args[1]));

                                } else if (targets.size() == 1) {
                                    Entity target = targets.get(0);
                                    boolean success = revokeCapability(target, capability);

                                    if (success) {
                                        sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.removed")
                                                .replaceAll("%s1", ChatColor.YELLOW + capability.getCapabilityName() + ChatColor.RESET)
                                                .replaceAll("%s2", target.getName()));

                                    } else
                                        sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.does_not_have"));

                                } else {
                                    boolean success = false;
                                    int effectedEntityCount = 0;

                                    for (Entity target : targets)
                                        if (revokeCapability(target, capability)) {
                                            success = true;
                                            effectedEntityCount++;
                                        }

                                    if (success) {
                                        sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.removed_multi")
                                                .replaceAll("%s1", ChatColor.YELLOW + capability.getCapabilityName() + ChatColor.RESET)
                                                .replaceAll("%s2", Integer.toString(effectedEntityCount)));

                                    } else
                                        sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.does_not_have_multi"));
                                }

                            } else
                                sender.sendMessage(ChatColor.RED + LocalizedMessages.getMessageFor(sender, "command.capabilities.not_known")
                                        .replaceAll("%s", args[2]));
                        }

                    } else
                        sender.sendMessage(LocalizedMessages.getMessageFor(sender, "command.capabilities.revoke.usage"));

                    return true;
            }

        return false;
    }

    /**
     * Logs a dump of the entity queue to the server and sends a copy to the admins through chat.
     */
    private static void logEntityQueueDump() {
        List<String> entityQueueDump = new ArrayList<>();

        // Grabs entities and their capabilities from the queue and adds it to the queue.
        for (Map.Entry<Entity, Set<Capability>> entityQueueEntry : ENTITY_CAPABILITY_QUEUE.entrySet()) {
            Entity entity = entityQueueEntry.getKey();
            Set<Capability> entityCapabilities = entityQueueEntry.getValue();

            entityQueueDump.add(" (" + ChatColor.YELLOW + entity.getType() + ChatColor.WHITE + ") " + entity.getName() + ":");

            for (Capability capability : entityCapabilities)
                entityQueueDump.add("  - " + ChatColor.YELLOW + joinNameAndExtra(capability.getCapabilityName(), capability.getExtraData()));
        }

        // Logs queue to server.
        Logger bukkitLogger = Bukkit.getLogger();

        bukkitLogger.info(LocalizedMessages.getMessage(LocalizedMessages.getDefaultLanguage(), "capabilities.debugger.dump_start"));

        if (entityQueueDump.isEmpty()) {
            bukkitLogger.info(LocalizedMessages.getMessage(LocalizedMessages.getDefaultLanguage(), "capabilities.debugger.empty"));

        } else
            for (String message : entityQueueDump)
                bukkitLogger.info(message);

        bukkitLogger.info(LocalizedMessages.getMessage(LocalizedMessages.getDefaultLanguage(), "capabilities.debugger.dump_stop"));

        // Sends logs to admins.
        for (OfflinePlayer operator : Bukkit.getOperators()) {
            Player admin = operator.getPlayer();

            if (admin != null) {
                admin.sendMessage(LocalizedMessages.getMessageFor(admin, "capabilities.debugger.dump_start"));

                if (entityQueueDump.isEmpty()) {
                    admin.sendMessage(LocalizedMessages.getMessageFor(admin, "capabilities.debugger.empty"));

                } else
                    admin.sendMessage((String[]) entityQueueDump.toArray());

                admin.sendMessage(LocalizedMessages.getMessageFor(admin, "capabilities.debugger.dump_stop"));
            }
        }
    }

    @Override
    public List<String> onTabComplete(CommandSender sender, Command command, String alias, String[] args) {
        List<String> arguments = new ArrayList<>();

        if (args.length == 1) {
            arguments.add("assign");
            arguments.add("revoke");
            arguments.add("list");
            arguments.add("debug");

        } else if (args.length >= 1)
            switch (args[0].toLowerCase()) {
                case "assign":
                    if (args.length == 2) {
                        arguments = null;

                    } else if (args.length == 3)
                        arguments.addAll(CAPABILITIES_REGISTRY.keySet());

                    break;

                case "revoke":
                    if (args.length == 2) {
                        arguments = null;

                    } else if (args.length == 3) {
                        List<Entity> targets = CommandHelper.getCommandTargets(sender, args[1]);
                        Set<Capability> targetsCapabilities = new HashSet<>();

                        for (Entity target : targets)
                            targetsCapabilities.addAll(getCapabilities(target));

                        for (Capability capability : targetsCapabilities)
                            arguments.add(capability.getCapabilityName());

                        if (!targetsCapabilities.isEmpty())
                            arguments.add("__all");
                    }

                    break;

                case "debug":
                    if (args.length == 2) {
                        arguments.add("start");
                        arguments.add("stop");
                        arguments.add("dump");
                        arguments.add("setInterval");
                    }

                    break;

                case "list":
                    if (args.length == 2)
                        arguments = null;
            }

        return arguments;
    }



    /**
     * An exception used for when there are duplicate registry names.
     */
    public static class DuplicateRegistryNameException extends Exception {
        DuplicateRegistryNameException(String message) {
            super(message);
        }
    }
}
