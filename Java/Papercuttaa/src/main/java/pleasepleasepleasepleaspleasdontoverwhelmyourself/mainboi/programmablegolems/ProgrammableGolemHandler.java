package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems;

import com.destroystokyo.paper.event.entity.EntityAddToWorldEvent;
import com.destroystokyo.paper.event.entity.EntityRemoveFromWorldEvent;
import org.bukkit.Location;
import org.bukkit.entity.ArmorStand;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.scheduler.BukkitRunnable;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.MainBoi;

import java.lang.Thread;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Code used to manage programmable golems.
 *
 * Launches another thread, and uses both it and the main thread to provide run loops for the golems.
 */
public final class ProgrammableGolemHandler implements Listener {
    // Storage of programmable golems in plugin memory for handling.
    static final ConcurrentHashMap<ArmorStand, ProgrammableGolemInstance> GOLEM_QUEUE = new ConcurrentHashMap<>();
    private static volatile boolean shouldStopThread = false;

    public static void onEnable() {
        // Asynchronous golem ticking.
        Thread golemCodeManagerAsynchronous = new Thread(() -> {
            long lastTime = System.nanoTime();

            while (!shouldStopThread) {
                for (ProgrammableGolemInstance programmableGolem : GOLEM_QUEUE.values())
                    if (programmableGolem.isOn)
                        programmableGolem.tick();

                // Makes thread run about every 1/60 of a second, unless it is behind schedule.
                long deltaTime = System.nanoTime() - lastTime;
                if (deltaTime < 16_666_670)
                    try {
                        Thread.sleep((16_666_670 - deltaTime) / 1_000_000);

                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }

                lastTime = System.nanoTime();
            }
        });

        // Synchronous golem runner
        BukkitRunnable golemCodeManagerSynchronous = new BukkitRunnable() {
            @Override
            public void run() {
                for (ProgrammableGolemInstance programmableGolem : GOLEM_QUEUE.values())
                    programmableGolem.synchronizedTick();
            }
        };

        golemCodeManagerAsynchronous.start();
        golemCodeManagerSynchronous.runTaskTimer(MainBoi.getInstance(), 0, 1);
    }

    public static void onDisable() {
        shouldStopThread = true;
    }



    /**
     * Spawns a programmable golem at the location, and returns its instance.
     *
     * @param location The location to spawn the golem at.
     *
     * @return The instance of the golem.
     */
    public static ProgrammableGolemInstance spawnProgrammableGolem(Location location) {
        ArmorStand golem = (ArmorStand) location.getWorld().spawnEntity(location, EntityType.ARMOR_STAND);

        golem.setCustomName("BotBoi #" + golem.getUniqueId().toString());
        golem.setCustomNameVisible(true);
        golem.setSmall(true);
        golem.setArms(true);
        golem.setBasePlate(false);
        golem.setDisabledSlots(EquipmentSlot.HEAD, EquipmentSlot.CHEST, EquipmentSlot.LEGS, EquipmentSlot.FEET, EquipmentSlot.HAND, EquipmentSlot.OFF_HAND);

        makeProgrammable(golem);

        return GOLEM_QUEUE.get(golem);
    }

    /**
     * Attempts to create a programmable golem instance for the given armor stand.
     *
     * @param armorStand The armor stand to turn into an active golem.
     *
     */
    private static void makeProgrammable(ArmorStand armorStand) {
        if (!GOLEM_QUEUE.containsKey(armorStand))
            if (armorStand.getScoreboardTags().contains("programmableGolem")) {
                GOLEM_QUEUE.put(armorStand, new ProgrammableGolemInstance(armorStand));

            } else {
                GOLEM_QUEUE.put(armorStand, new ProgrammableGolemInstance(armorStand));
                armorStand.addScoreboardTag("programmableGolem");
            }
    }


    /**
     * Attempts to revert the given golem back into an armor stand.
     *
     * @param golem The golem to revert.
     *
     */
    static void makeNotProgrammable(ArmorStand golem) {
        if (GOLEM_QUEUE.containsKey(golem)) {
            GOLEM_QUEUE.remove(golem);
            golem.removeScoreboardTag("programmableGolem");
        }
    }



    //////////////////////////////////////////////////////////////////////////////////////////////
    // The following 2 event handlers control the loading and unloading of programmable golems. //
    //////////////////////////////////////////////////////////////////////////////////////////////
    @EventHandler
    public static void onEntityLoad(EntityAddToWorldEvent entityAddToWorldEvent) {
        Entity possibleGolem = entityAddToWorldEvent.getEntity();

        if (possibleGolem instanceof ArmorStand && possibleGolem.getScoreboardTags().contains("programmableGolem"))
            makeProgrammable((ArmorStand) possibleGolem);
    }

    @EventHandler
    public static void onEntityUnload(EntityRemoveFromWorldEvent entityRemoveFromWorldEvent) {
        Entity possibleGolem = entityRemoveFromWorldEvent.getEntity();

        if (possibleGolem instanceof ArmorStand)
            GOLEM_QUEUE.remove(possibleGolem);
    }
}
