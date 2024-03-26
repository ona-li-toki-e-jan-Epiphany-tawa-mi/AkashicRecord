package mannyboidpaperrus.paperthinger.bleblocks;

import mannyboidpaperrus.paperthinger.standardstuffs.MiscPaperConstants;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.data.type.Door;
import org.bukkit.entity.ArmorStand;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.block.BlockRedstoneEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;

/**
 * Handles blocks.
 */
public class BlockHandler implements Listener {
    /**
     * Opens up both of the doors on a double door with one click.
     * Does not work with iron doors.
     */
    @EventHandler
    public static void doubleDoorOpener(PlayerInteractEvent playerInteractEvent) {
        Action action = playerInteractEvent.getAction();
        Block block = playerInteractEvent.getClickedBlock();

        if (action.equals(Action.RIGHT_CLICK_BLOCK) && block != null && !block.getType().equals(Material.IRON_DOOR) && MiscPaperConstants.doors.contains(block.getType())) {
            Door doorMeta = (Door) block.getBlockData();
            Block otherDoor = getOtherDoor(block);

            if (otherDoor != null && !otherDoor.getType().equals(Material.IRON_DOOR)) {
                Door otherDoorData = (Door) otherDoor.getBlockData();

                if (doorMeta.isOpen())
                    otherDoorData.setOpen(false);
                else
                    otherDoorData.setOpen(true);

                if (!playerInteractEvent.isCancelled())
                    otherDoor.setBlockData(otherDoorData);
            }
        }
    }

    /**
     * Gets the other door of a double door, or null, if it doesn't exist.
     *
     * @param door The original door to find the partner of.
     *
     * @return The other door of a double door, or null, if it doesn't exist.
     */
    private static Block getOtherDoor(Block door) {
        Door doorMeta = (Door) door.getBlockData();
        BlockFace facing = doorMeta.getFacing();
        Door.Hinge hinge = doorMeta.getHinge();
        World world = door.getWorld();
        Block possibleDoor;

        switch (facing) {
            case NORTH:
                if (hinge.equals(Door.Hinge.LEFT))
                    possibleDoor = world.getBlockAt(door.getLocation().add(1, 0, 0));
                else
                    possibleDoor = world.getBlockAt(door.getLocation().add(-1, 0, 0));
                break;

            case SOUTH:
                if (hinge.equals(Door.Hinge.LEFT))
                    possibleDoor = world.getBlockAt(door.getLocation().add(-1, 0, 0));
                else
                    possibleDoor = world.getBlockAt(door.getLocation().add(1, 0, 0));
                break;

            case EAST:
                if (hinge.equals(Door.Hinge.LEFT))
                    possibleDoor = world.getBlockAt(door.getLocation().add(0, 0, 1));
                else
                    possibleDoor = world.getBlockAt(door.getLocation().add(0, 0, -1));
                break;

            case WEST:
                if (hinge.equals(Door.Hinge.LEFT))
                    possibleDoor = world.getBlockAt(door.getLocation().add(0, 0, -1));
                else
                    possibleDoor = world.getBlockAt(door.getLocation().add(0, 0, 1));
                break;

            default:
                return null;
        }

        if (MiscPaperConstants.doors.contains(possibleDoor.getType())) {
            Door possibleDoorMeta = (Door) possibleDoor.getBlockData();

            if (!possibleDoorMeta.getHinge().equals(hinge) && possibleDoorMeta.getFacing().equals(facing)) {
                return possibleDoor;
            }
        }

        return null;
    }

    /**
     * Causes doors of double doors when triggered with redstone to open up their partner doors.
     */
    @EventHandler
    public static void onDoorPulse(BlockRedstoneEvent blockRedstoneEvent) {
        Block block = blockRedstoneEvent.getBlock();
        int current = blockRedstoneEvent.getNewCurrent();

        if (MiscPaperConstants.doors.contains(block.getType())) {
            Block otherDoor = getOtherDoor(block);

            if (otherDoor != null) {
                Door otherDoorData = (Door) otherDoor.getBlockData();

                if (current == 0)
                    otherDoorData.setOpen(false);
                else
                    otherDoorData.setOpen(true);

                otherDoor.setBlockData(otherDoorData);
            }
        }
    }

    /**
     * Causes a player to sit in a stair if they right-click.
     */
    @EventHandler
    public static void onPlayerTrySit(PlayerInteractEvent playerInteractEvent) {
        Action action = playerInteractEvent.getAction();
        Block block = playerInteractEvent.getClickedBlock();
        ItemStack item = playerInteractEvent.getItem();
        EquipmentSlot hand = playerInteractEvent.getHand();

        if (action.equals(Action.RIGHT_CLICK_BLOCK) && hand != null && hand.equals(EquipmentSlot.HAND) && block != null && item == null) {
            Player player = playerInteractEvent.getPlayer();
            World world = player.getWorld();

            if (MiscPaperConstants.stairs.contains(block.getType()) && !playerInteractEvent.isCancelled()) {
                playerInteractEvent.setCancelled(true);

                ArmorStand armorStand = (ArmorStand) world.spawnEntity(block.getLocation().add(0.5, 0.25, 0.5), EntityType.ARMOR_STAND);

                armorStand.setMarker(true);
                armorStand.setVisible(false);
                armorStand.setCustomName("paperthinger_armorstand_chair");
                armorStand.setCustomNameVisible(false);

                armorStand.addPassenger(player);
            }
        }
    }

    /**
     * Runs over entities.
     *
     * @param entity The entity currently being checked on the loop.
     */
    public static void entityLoop(Entity entity) {
        if (entity instanceof ArmorStand && entity.getCustomName() != null && entity.getCustomName().equals("paperthinger_armorstand_chair")) {
            ArmorStand armorStand = (ArmorStand) entity;

            if (armorStand.getPassengers().size() == 0)
                armorStand.remove();
        }
    }
}
