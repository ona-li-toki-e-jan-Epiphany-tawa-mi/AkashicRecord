package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.ArmorStand;
import org.bukkit.entity.Entity;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.player.PlayerInteractAtEntityEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.plugin.PluginManager;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.MainBoi;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.ClassDefinition;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.ClassInstance;

import java.util.ArrayList;
import java.util.UUID;

public class ProgrammableGolemInstance implements Listener {
    private final ArmorStand golem;
    private final Inventory golemInventory;

    final ArrayList<Thread> threads;
    public final ArrayList<ClassInstance> heapMemory;
    private final ArrayList<ClassDefinition> methodArea;

    volatile boolean isOn = false;
    private boolean isStopped = false;

    protected ProgrammableGolemInstance(ArmorStand golem) {
        this.golem = golem;
        golemInventory = Bukkit.createInventory(null, 36, golem.getCustomName() + "'s Inventory");

        threads = new ArrayList<>();
        threads.add(new Thread(this));

        heapMemory = new ArrayList<>();
        methodArea = new ArrayList<>();
    }

    protected void tick() {
        for (int i = 0; i < threads.size(); i++)
            threads.get(i).runThread();
    }

    protected void synchronizedTick() {
        if (golem.isDead())
            ProgrammableGolemHandler.makeNotProgrammable(golem);
    }



    /**
     * Only used for adding events.
     */
    private ProgrammableGolemInstance() {
        golem = null;
        golemInventory = null;
        threads = null;
        heapMemory = null;
        methodArea = null;
    }

    public static void onEnable() {
        PluginManager pluginManager = Bukkit.getPluginManager();
        pluginManager.registerEvents(new ProgrammableGolemInstance(), MainBoi.getInstance());
    }


    /**
     * Displays a GUI for players interacting with programmable golems.
     */
    @EventHandler(ignoreCancelled = true)
    public static void OnPlayerClickGolem(PlayerInteractAtEntityEvent playerInteractAtEntityEvent) {
        Entity possibleGolem = playerInteractAtEntityEvent.getRightClicked();

        if (possibleGolem instanceof ArmorStand && ProgrammableGolemHandler.GOLEM_QUEUE.containsKey(possibleGolem)) {
            String customName = possibleGolem.getCustomName();
            String title = customName == null ? "BotBoi #" + possibleGolem.getUniqueId().toString() + " Manager" : customName + " Manager";

            Inventory golemGUI = Bukkit.createInventory(playerInteractAtEntityEvent.getPlayer(), 9, title);
            ProgrammableGolemInstance programmableGolem = ProgrammableGolemHandler.GOLEM_QUEUE.get(possibleGolem);


            // Adds items to GUI.
            ItemStack powerButton;
            ItemStack placeHolder = new ItemStack(Material.GRAY_STAINED_GLASS_PANE);
            ItemStack golemInventory = new ItemStack(Material.CHEST);
            ItemStack codeManager = new ItemStack(Material.WRITTEN_BOOK);

            if (programmableGolem.isOn) {
                powerButton = new ItemStack(Material.BLACK_TERRACOTTA);

                ItemMeta powerButtonMeta = powerButton.getItemMeta();
                powerButtonMeta.setDisplayName("Stop Golem");
                powerButton.setItemMeta(powerButtonMeta);

            } else {
                powerButton = new ItemStack(Material.GREEN_TERRACOTTA);

                ItemMeta powerButtonMeta = powerButton.getItemMeta();
                powerButtonMeta.setDisplayName("Start Golem");
                powerButton.setItemMeta(powerButtonMeta);
            }

            ItemMeta golemInventoryMeta = golemInventory.getItemMeta();
            golemInventoryMeta.setDisplayName("Open Golem Inventory");
            golemInventory.setItemMeta(golemInventoryMeta);

            ItemMeta codeManagerMeta = codeManager.getItemMeta();
            codeManagerMeta.setDisplayName("Open Golem Code Manager");
            codeManager.setItemMeta(codeManagerMeta);

            golemGUI.addItem(placeHolder, placeHolder, powerButton, placeHolder, placeHolder, golemInventory, codeManager, placeHolder, placeHolder);
            System.out.println("Nyahello~");
            playerInteractAtEntityEvent.setCancelled(true);
        }
    }

    @EventHandler(ignoreCancelled = true)
    public static void onInventoryClick(InventoryClickEvent inventoryClickEvent) {
        String title = inventoryClickEvent.getView().getTitle();

        if (title.contains("BotBoi") && title.contains("Manager")) {
            ItemStack currentItem = inventoryClickEvent.getCurrentItem();

            if (currentItem != null) {
                ArmorStand possibleGolem = (ArmorStand) Bukkit.getEntity(
                        UUID.fromString(title.split(" ", 3)[1].split("#", 2)[1])
                );

                // Runs effects for the selected item.
                if (possibleGolem != null) {
                    ProgrammableGolemInstance programmableGolem = ProgrammableGolemHandler.GOLEM_QUEUE.get(possibleGolem);
                    Material currentItemType = currentItem.getType();

                    // Off button.
                    if (currentItemType.equals(Material.BLACK_TERRACOTTA)) {
                        programmableGolem.isOn = false;
                        programmableGolem.isStopped = false;

                        // Updates GUI.
                        Inventory clickedInventory = inventoryClickEvent.getClickedInventory();

                        if (clickedInventory != null) {
                            ItemStack onButton = new ItemStack(Material.GREEN_TERRACOTTA);

                            ItemMeta onButtonMeta = onButton.getItemMeta();
                            onButtonMeta.setDisplayName("Start Golem");
                            onButton.setItemMeta(onButtonMeta);

                            inventoryClickEvent.getClickedInventory().setItem(0, onButton);
                        }

                        // On button.
                    } else if (currentItemType.equals(Material.GREEN_TERRACOTTA)) {
                        programmableGolem.isOn = true;
                        programmableGolem.isStopped = false;

                        // Updates GUI.
                        Inventory clickedInventory = inventoryClickEvent.getClickedInventory();

                        if (clickedInventory != null) {
                            ItemStack offButton = new ItemStack(Material.BLACK_TERRACOTTA);

                            ItemMeta offButtonMeta = offButton.getItemMeta();
                            offButtonMeta.setDisplayName("Stop Golem");
                            offButton.setItemMeta(offButtonMeta);

                            inventoryClickEvent.getClickedInventory().setItem(0, offButton);
                        }

                        // Inventory button.
                    } else if (currentItemType.equals(Material.CHEST)) {
                        inventoryClickEvent.getWhoClicked().openInventory(programmableGolem.golemInventory);

                        // Code manager button.
                    } else if (currentItemType.equals(Material.WRITTEN_BOOK)) {

                    }
                }
            }

            inventoryClickEvent.setCancelled(true);
        }
    }
}
