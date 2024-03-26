package me.trial.gang;

import com.comphenix.protocol.PacketType;
import com.comphenix.protocol.ProtocolLibrary;
import com.comphenix.protocol.ProtocolManager;
import com.comphenix.protocol.events.ListenerPriority;
import com.comphenix.protocol.events.PacketAdapter;
import com.comphenix.protocol.events.PacketEvent;
import me.trial.gang.customEnchantsss.GlowEnchant;
import org.bukkit.*;
import org.bukkit.attribute.Attribute;
import org.bukkit.block.Block;
import org.bukkit.block.data.BlockData;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.*;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.entity.*;
import org.bukkit.event.player.*;
import org.bukkit.inventory.*;
import org.bukkit.inventory.meta.Damageable;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.material.Crops;
import org.bukkit.material.NetherWarts;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.potion.PotionEffect;
import org.bukkit.potion.PotionEffectType;
import org.bukkit.scheduler.BukkitRunnable;
import org.bukkit.util.Vector;

import java.lang.reflect.Field;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public final class Gang extends JavaPlugin implements Listener {
    private static World overworld, nether, end;
    private ProtocolManager plpm;
    private static final String[] deathMessage = {" got what they deserved", " got rekt epic minecraft style",
            " was silenced", " went out for some cigarettes", " got nay-nayed"};
    private static final Material[] carpets = {Material.BLACK_CARPET, Material.CYAN_CARPET, Material.BLUE_CARPET,
            Material.BROWN_CARPET, Material.GRAY_CARPET, Material.GREEN_CARPET, Material.LIGHT_BLUE_CARPET,
            Material.LIGHT_GRAY_CARPET, Material.LIME_CARPET, Material.MAGENTA_CARPET, Material.ORANGE_CARPET,
            Material.PINK_CARPET, Material.PURPLE_CARPET, Material.RED_CARPET, Material.WHITE_CARPET, Material.YELLOW_CARPET};
    private static Gang instance;
    private static boolean sunOnline;
    private ConfigManager cfgm;
    private static final String[] morningMessage = {"<The_Sun> Good morning!", "<The_Sun> Hey Vsauce, Micheal here!",
            "<The_Sun> Hi sisters!!", "<The_Sun> Where are your fingers?", "<The_Sun> Good morning Bella!"};
    private static final String[] sunsetMessage = {"<The_Sun> Good night!", "<The_Sun> RRREEEEEEEEEEEEEE!!!11!",
            "* The_Moon sighs", "<The_Sun> I'MM PICKLE RRRIIIICKKKKKK!!!"};
    private static boolean tOvrD = false; // Tells whether or not the aligned time cycle is being overridden
    private static boolean tOvrD2 = false; // Tells whether the overridden time cycle is at day
    private static boolean tOvrD3 = false; // Tells whether the overridden time cycle is at night
    private static final String[] diseaseTags = {"suddenDeath", "hemophilia", "lactoseInt", "celiacs", "pBlindness",
            "blindness", "deafness"};
    private static final double[] diseaseChance = {0.0912, 0.1, 7, 1, 0.8, 0.4, 0.3};
    private static boolean sunOverrideEnabled = false;
    public static NamespacedKey kGlow;
    private static GlowEnchant glow;
    private static String[] customEnchants = {ChatColor.RED + "Curse of Constraint", ChatColor.GRAY + "Vorpal Edge", ChatColor.GRAY + "Luminescence"};
    private static int[] customEnchantWeights = {1, 2, 1};


    @Override
    public void onEnable() {
        kGlow = new NamespacedKey(this, "useless_enchant");
        glow = new GlowEnchant();
        overworld = getServer().getWorld("world");
        nether = getServer().getWorld("world_nether");
        end = getServer().getWorld("world_the_end");
        plpm = ProtocolLibrary.getProtocolManager();
        instance = this;
        long runspeed = 200;

        getServer().getPluginManager().registerEvents(this, this);

        if (sunOverrideEnabled)
            overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, false);

        loadConfigManager();

        getServer().dispatchCommand(getServer().getConsoleSender(), "function extras:installextras");

        try {
            try {
                Field f = Enchantment.class.getDeclaredField("acceptingNew");
                f.setAccessible(true);
                f.set(null, true);
            } catch (Exception e) {
                e.printStackTrace();
            }
            try {
                Enchantment.registerEnchantment(glow);
            } catch (IllegalArgumentException ignored) {}
        } catch(Exception e){
            e.printStackTrace();
        }

        new BukkitRunnable() {
            boolean sunMessage = false;
            long currentTime;
            int S;
            int chooseName;
            int previousTime1 = 1000000;
            long previousTime2 = 1000000;

            @Override
            public void run() {
                S = (int) Math.floor(((conv("HH") * 1000) - 6000) + (conv("mm") * 16.7)); // Converts real time to minecraft time
                currentTime = overworld.getTime();

                if (previousTime1 != S || previousTime2 != currentTime) {
                    if (currentTime > 23000 || currentTime < 13900)
                        sunOnline = true;
                    else if (currentTime > 13900 && currentTime < 23000)
                        sunOnline = false;

                    if (currentTime >= 22900 && currentTime <= 23100) {
                        if (!sunMessage) { // Only makes the message happen once per sunrise
                            chooseName = (int) Math.floor(Math.random() * (morningMessage.length - 1));

                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage(ChatColor.YELLOW + "The_Sun joined the game"));
                            new BukkitRunnable() {
                                @Override
                                public void run() {
                                    getServer().getOnlinePlayers().forEach(p -> p.sendMessage(morningMessage[chooseName]));
                                }
                            }.runTaskLater(instance, 20);
                            sunMessage = true;
                        }
                    } else if (currentTime >= 13800 && currentTime <= 14000) {
                        if (!sunMessage) { // Only makes the message happen once per sunset
                            chooseName = (int) Math.floor(Math.random() * (sunsetMessage.length - 1));

                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage(sunsetMessage[chooseName]));
                            new BukkitRunnable() {
                                @Override
                                public void run() {
                                    getServer().getOnlinePlayers().forEach(p -> p.sendMessage(ChatColor.YELLOW + "The_Sun left the game"));
                                }
                            }.runTaskLater(instance, 20);
                            sunMessage = true;
                        }
                    } else {
                        sunMessage = false;
                    }

                    if (sunOverrideEnabled)
                        if (!tOvrD)
                            overworld.setTime(S);
                        else if (!tOvrD3 && tOvrD2 && currentTime >= S - 500 && currentTime <= S + 500) {
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Sun> Wow, I feel so happy and encouraged today, it would be rather unfortunate if someone came along and ruined it"));
                            overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, false);
                            overworld.setTime(S);
                            tOvrD = false;
                            tOvrD2 = false;
                        } else if (!tOvrD2 && currentTime >= S - 500 && currentTime <= S + 500) {
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Moon> ..."));
                            overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, false);
                            overworld.setTime(S);
                            tOvrD = false;
                            tOvrD3 = false;
                        }

                    previousTime1 = S;
                    previousTime2 = currentTime;
                }
            }
        }.runTaskTimer(this, 20, runspeed);

        plpm.addPacketListener(new PacketAdapter(this, ListenerPriority.NORMAL, PacketType.Play.Server.NAMED_SOUND_EFFECT, PacketType.Play.Server.CUSTOM_SOUND_EFFECT) {
            @Override
            public void onPacketSending(PacketEvent e) {
                Player p = e.getPlayer();

                if (p.getScoreboardTags().size() > 0 && p.getScoreboardTags().contains("deafness")) {
                        e.setCancelled(true);
                    }
            }
        });
    }



    @Override
    public boolean onCommand(CommandSender sender, Command command, String s, String[] args) {
        Player player = (Player) sender;

        if (command.getName().equalsIgnoreCase("tornado")) {
            if (args.length > 1)
                player.sendMessage(ChatColor.RED + "This command doesn't accept any arguments");
            else {
                long totalTime = 10000;
                long startTime;
                boolean direction = Math.floor(Math.random() * 2) == 1;
                startTime = System.currentTimeMillis();
                Tornado tor = new Tornado(player.getLocation(), 5);

                tor.setDensity(5);

                new BukkitRunnable() {
                    @Override
                    public void run() {
                        tor.moveTornado(0.05, 0, 0.05);
                        tor.runTornado(Particle.CLOUD, direction);

                        if (System.currentTimeMillis() - startTime >= totalTime) {
                            cancel();
                        }
                    }
                }.runTaskTimer(this, 0, 1);
            }
        } else if (command.getName().equalsIgnoreCase("floss")) {
            if (args.length > 1)
                player.sendMessage(ChatColor.RED + "This command doesn't accept any arguments");
            else {
                player.setHealth(0);
                player.sendMessage("No fortnite");
            }
        }

        return true;
    }



    @EventHandler
    public void chatCheck(AsyncPlayerChatEvent e) {
        String message = e.getMessage();
        Player p = e.getPlayer();
        World bigW = p.getWorld();

        if (sunOverrideEnabled)
            if (message.toLowerCase().contains("bad sun") && !tOvrD && p.getWorld() == overworld && p.getLocation().getY() >= 256 &&
                    overworld.getTime() >= 0 && overworld.getTime() <= 12000 && p.getLocation().getPitch() == -90) {
                new BukkitRunnable() {
                        int cntr = 0;

                    @Override
                    public void run() {
                        if (cntr < 1)
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Sun> >:("));
                        else if (cntr < 2)
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Sun> That's pretty rude"));
                        else if (cntr < 3)
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Sun> Meanie"));
                        else if (cntr < 4)
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Sun> ..."));
                        else if (cntr < 5) {
                            getServer().getOnlinePlayers().forEach(p -> p.sendMessage(ChatColor.YELLOW + "The_Sun left the game"));
                        tOvrD = true;
                        tOvrD2 = true;
                        overworld.setTime(16406);
                        overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, true);
                        cancel();
                        }

                        cntr++;
                    }
                }.runTaskTimer(this, 400, 40);
        }

        if (message.toLowerCase().contains("nigger") || message.toLowerCase().contains("negro") || message.toLowerCase().contains("nigga")) {
            boolean heGood = false;

            ItemStack testorino;
            for (int x = 0; x < p.getInventory().getContents().length; x++) {
                testorino = p.getInventory().getItem(x);

                if (testorino != null && testorino.getType() == Material.PAPER && testorino.hasItemMeta() &&
                        testorino.getItemMeta().hasDisplayName() && testorino.getItemMeta().getDisplayName().toLowerCase().contains("n-word pass")) {
                                heGood = true;
                                break;
                            }
            }

            if (!heGood) {
                e.setCancelled(true);

                if (sunOnline && bigW == overworld)
                    getServer().getOnlinePlayers().forEach(pp -> p.sendMessage("<The_Sun> Whoa there bucko!, " + ChatColor.DARK_GREEN + p.getDisplayName() + ChatColor.WHITE + " used the " + ChatColor.RED + "N-Word " + ChatColor.WHITE + "without an N-word pass!"));
                else
                    getServer().getOnlinePlayers().forEach(pp -> p.sendMessage("<Skipper> MRS. OBAMA GET DOWN!, " + ChatColor.DARK_GREEN + p.getDisplayName().toUpperCase() + ChatColor.WHITE + " USED THE " + ChatColor.RED + "N-WORD!!!11!"));
                bigW.setGameRule(GameRule.SHOW_DEATH_MESSAGES, false);
               
                new BukkitRunnable() {
                    @Override
                    public void run() {
                        if (p.getGameMode() == GameMode.SURVIVAL || p.getGameMode() == GameMode.ADVENTURE)
                            p.damage(p.getHealth() * p.getAttribute(Attribute.GENERIC_MAX_HEALTH).getValue());
                        else
                            p.setHealth(0);
                        getServer().getOnlinePlayers().forEach(p -> p.sendMessage(p.getDisplayName() + deathMessage[(int) Math.floor(Math.random() * (deathMessage.length - 1))]));
                        bigW.setGameRule(GameRule.SHOW_DEATH_MESSAGES, true);

                        new BukkitRunnable() {
                            @Override
                            public void run() {
                                if (sunOnline && bigW == overworld)
                                    getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<The_Sun> Hopefully " + p.getDisplayName() + " learnt their lesson about no freaking cussing on my fricken' christan minecraft server!"));
                                else
                                    getServer().getOnlinePlayers().forEach(p -> p.sendMessage("<Skipper> Well boys, we did it. Racism is no more"));
                            }
                        }.runTaskLater(instance, 40);
                    }
                }.runTaskLater(this, 50);
            }
        }
    }



    @EventHandler
    public void onEntityDamageEntity(EntityDamageByEntityEvent e) {
        if (e.getDamager().getType() == EntityType.LIGHTNING || e.getEntity().getType() == EntityType.DROPPED_ITEM)
            return;

        Entity chad = e.getDamager();
        LivingEntity sal = (LivingEntity)e.getEntity();

        if (chad instanceof Player && (sal.getType() == EntityType.SPIDER || sal.getType() == EntityType.CAVE_SPIDER)) {
                boolean weBowlin = false;
                for (Material carpet : carpets)
                    if (((Player) chad).getInventory().getItemInMainHand().getType() == carpet) {
                        weBowlin = true;
                        break;
                    }

                if (weBowlin)
                    sal.damage(sal.getHealth() * sal.getAttribute(Attribute.GENERIC_MAX_HEALTH).getValue());
            }
        if (chad instanceof Player && sal instanceof Animals) {
                PotionEffectManager.tryApplyPotionEffect(sal, new PotionEffect(PotionEffectType.SPEED, 200, 0, false, false), true);
                PotionEffectManager.tryApplyPotionEffect(sal, new PotionEffect(PotionEffectType.JUMP, 200, 0, false, false), true);
                if (sal.getType() == EntityType.WOLF || sal.getType() == EntityType.POLAR_BEAR)
                    PotionEffectManager.tryApplyPotionEffect(sal, new PotionEffect(PotionEffectType.INCREASE_DAMAGE, 200, 0, false, false), true);
            }

        Location loc = e.getEntity().getLocation();
        Location loc1 = e.getDamager().getLocation();
        if (loc.getDirection().dot(new Vector(loc1.getX() - loc.getX(), loc1.getY() - loc.getY(), loc1.getZ() - loc.getZ()).normalize()) <= -0.6) {
            if (sal instanceof Player) {
                ItemStack armir = ((Player) sal).getInventory().getChestplate();

                if (armir == null) {
                    ((Player) sal).playSound(loc, Sound.ITEM_ARMOR_EQUIP_GENERIC, SoundCategory.PLAYERS, 1, (float) (0.7 + (Math.random() / 10)));
                    e.setDamage(e.getDamage() * 1.5);
                } else if (armir.getType() == Material.LEATHER_CHESTPLATE) {
                    ((Player) sal).playSound(loc, Sound.ITEM_ARMOR_EQUIP_LEATHER, SoundCategory.PLAYERS, 1, (float) (0.7 + (Math.random() / 10)));
                    e.setDamage(e.getDamage() * 1.45);
                } else if (armir.getType() == Material.GOLDEN_CHESTPLATE) {
                    ((Player) sal).playSound(loc, Sound.ITEM_ARMOR_EQUIP_GOLD, SoundCategory.PLAYERS, 1, (float) (0.7 + (Math.random() / 10)));
                    e.setDamage(e.getDamage() * 1.42);
                } else if (armir.getType() == Material.CHAINMAIL_CHESTPLATE) {
                    ((Player) sal).playSound(loc, Sound.ITEM_ARMOR_EQUIP_CHAIN, SoundCategory.PLAYERS, 1, (float) (0.7 + (Math.random() / 10)));
                    e.setDamage(e.getDamage() * 1.38);
                } else if (armir.getType() == Material.IRON_CHESTPLATE) {
                    ((Player) sal).playSound(loc, Sound.ITEM_ARMOR_EQUIP_IRON, SoundCategory.PLAYERS, 1, (float) (0.7 + (Math.random() / 10)));
                    e.setDamage(e.getDamage() * 1.30);
                } else if (armir.getType() == Material.DIAMOND_CHESTPLATE) {
                    ((Player) sal).playSound(loc, Sound.ITEM_ARMOR_EQUIP_DIAMOND, SoundCategory.PLAYERS, 1, (float) (0.7 + (Math.random() / 10)));
                    e.setDamage(e.getDamage() * 1.22);
                }
            } else if (sal.getType() == EntityType.ZOMBIE || sal.getType() == EntityType.ZOMBIE_VILLAGER || sal.getType() == EntityType.PIG_ZOMBIE ||
                    sal.getType() == EntityType.SKELETON || sal.getType() == EntityType.STRAY || sal.getType() == EntityType.HUSK ||
                    sal.getType() == EntityType.DROWNED || sal.getType() == EntityType.WITHER_SKELETON || sal.getType() == EntityType.GIANT) {
                ItemStack armir = sal.getEquipment().getChestplate();

                if (armir == null)
                    e.setDamage(e.getDamage() * 1.5);
                else if (armir.getType() == Material.LEATHER_CHESTPLATE)
                    e.setDamage(e.getDamage() * 1.45);
                else if (armir.getType() == Material.GOLDEN_CHESTPLATE)
                    e.setDamage(e.getDamage() * 1.42);
                else if (armir.getType() == Material.CHAINMAIL_CHESTPLATE)
                    e.setDamage(e.getDamage() * 1.38);
                else if (armir.getType() == Material.IRON_CHESTPLATE)
                    e.setDamage(e.getDamage() * 1.30);
                else if (armir.getType() == Material.DIAMOND_CHESTPLATE)
                    e.setDamage(e.getDamage() * 1.22);
            } else
                e.setDamage(e.getDamage() * 1.5);

            if (chad instanceof Player)
                ((Player) chad).playSound(loc, Sound.ENTITY_ARROW_HIT_PLAYER, SoundCategory.PLAYERS, 1, 1);
            else if (chad instanceof Arrow)
                if (((Arrow) chad).getShooter() instanceof Player)
                    (((Player) ((Arrow) chad).getShooter())).playSound(loc, Sound.ENTITY_ARROW_HIT_PLAYER, SoundCategory.PLAYERS, 1, 1);
        }
    }



    @EventHandler
    public void onEntitySpawn(EntitySpawnEvent e) {
        Entity lilDude = e.getEntity();

        if (lilDude.getWorld() == overworld) {
            if (lilDude.getScoreboardTags().size() > 0) {
                if (!lilDude.getScoreboardTags().contains("overworld"))
                    lilDude.addScoreboardTag("overworld");
            } else
                lilDude.addScoreboardTag("overworld");
        } else
            if (lilDude.getScoreboardTags().size() > 0)
                if (lilDude.getScoreboardTags().contains("overworld"))
                    lilDude.removeScoreboardTag("overworld");

        boolean timeCycleAlignedBuff = lilDude.getWorld() != overworld || !sunOnline || (lilDude.getLocation().getY() < 30 &&
                (lilDude.getLocation().getBlock().getType() == Material.CAVE_AIR || lilDude.getLocation().getBlock().getType() == Material.CAVE_AIR));

        if (timeCycleAlignedBuff)
            if (lilDude.getType() == EntityType.ZOMBIE) {
                if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.ZOMBIE);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.ZOMBIE);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.ZOMBIE);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.ZOMBIE);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.ZOMBIE);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.ZOMBIE);
                }
            } else if (lilDude.getType() == EntityType.DROWNED) {
                if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.DROWNED);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.DROWNED);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.DROWNED);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.DROWNED);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.DROWNED);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.DROWNED);
                }
            } else if (lilDude.getType() == EntityType.HUSK) {
                if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.HUSK);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.HUSK);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.HUSK);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.HUSK);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.HUSK);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.HUSK);
                }
            } else if (lilDude.getType() == EntityType.CREEPER) {
                if (Math.floor(Math.random() * 10) + 1 == 1)
                    ((Creeper) lilDude).setMaxFuseTicks(((Creeper) lilDude).getMaxFuseTicks() / 3);
            } else if (lilDude.getType() == EntityType.PIG_ZOMBIE) {
                if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.PIG_ZOMBIE);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.PIG_ZOMBIE);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.PIG_ZOMBIE);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.PIG_ZOMBIE);
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.PIG_ZOMBIE);
                } else if (Math.random() <= 0.1) {
                    lilDude.getWorld().spawnEntity(lilDude.getLocation(), EntityType.PIG_ZOMBIE);
                }
            }

        if (lilDude instanceof Animals && ((Animals)lilDude).isAdult() &&Math.random() < 0.33) {
            Animals ani = ((Animals)lilDude);
            ani.setBaby();
        }
    }



    @EventHandler
    public void onPlayerJoin(PlayerJoinEvent e) {
        Player p = e.getPlayer();
        Location loc = p.getLocation();
        boolean ovrW = false;

        if (!p.hasPlayedBefore()) {
            loc.setY(255);
            p.teleport(loc);
            PotionEffectManager.tryApplyPotionEffect(p, new PotionEffect(PotionEffectType.DAMAGE_RESISTANCE, 500, 4, true, true), true);
            if (p.isFlying())
                p.setFlying(false);
        }

        if (loc.getWorld() == overworld) {
            p.sendTitle(ChatColor.DARK_GREEN + "The Overworld", ChatColor.GREEN + "" + ChatColor.ITALIC + "Life's Sanctuary", 40, 100, 30);
            ovrW = true;

            if (p.getScoreboardTags().size() > 0) {
                if (!p.getScoreboardTags().contains("overworld"))
                    p.addScoreboardTag("overworld");
            } else
                p.addScoreboardTag("overworld");
        } else
        if (p.getScoreboardTags().size() > 0)
            if (p.getScoreboardTags().contains("overworld"))
                p.removeScoreboardTag("overworld");

        if (!ovrW)
            if (loc.getWorld() == nether)
                p.sendTitle(ChatColor.DARK_RED + "The Nether", ChatColor.GOLD + "" + ChatColor.ITALIC + "Visceral Hell", 40, 100, 30);
            else if (loc.getWorld() == end)
                p.sendTitle(ChatColor.DARK_PURPLE + "The End", ChatColor.BLACK + "" + ChatColor.BOLD + "" + ChatColor.ITALIC + "" + ChatColor.MAGIC + "???", 40, 100, 30);

    }



    @EventHandler
    public void woke(PlayerBedEnterEvent e) {
        if (sunOverrideEnabled)
            new BukkitRunnable() {
                int playerCount = overworld.getPlayers().size();
                int playerCountS = 0;
                double coef = 0.5;

                @Override
                public void run() {
                    for (Player p : overworld.getPlayers())
                        if (p.isSleeping())
                            playerCountS++;

                    if ((double) playerCountS / playerCount > coef && !tOvrD3) {
                        Location loc;
                        int chooseName = (int) Math.ceil(Math.random() * (morningMessage.length - 1));
                        getServer().getOnlinePlayers().forEach(p -> p.sendMessage(morningMessage[chooseName]));
                        tOvrD = true;
                        overworld.setTime(23000);
                        overworld.setStorm(false);
                        overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, true);
                        tOvrD3 = true;
                        for (Player p : overworld.getPlayers())
                            if (p.isSleeping()) {
                                loc = p.getLocation();
                                loc.setY(loc.getY() + 1);
                                overworld.spawnEntity(loc, EntityType.SNOWBALL);
                            }
                    } else {
                        getServer().getOnlinePlayers().forEach(p -> p.sendMessage(ChatColor.YELLOW + "There are " + ChatColor.WHITE + playerCountS + ChatColor.YELLOW + " people sleeping, " + ChatColor.WHITE + Math.ceil(playerCount * coef) + ChatColor.YELLOW + " are needed for day. (" + ((int)((playerCountS / playerCount) *10.0))/10.0 + "%)"));
                    }

                }
            }.runTaskLater(this, 1);
    }



    @EventHandler
    public void onPlayerRespawn(PlayerRespawnEvent e) {
        Player p = e.getPlayer();
        Location loc = e.getRespawnLocation();
        SecureRandom sr = new SecureRandom();

        for (int x = 0; x < diseaseTags.length; x++)
            if (sr.nextInt(10000) / 100.0 <= diseaseChance[x])
                p.addScoreboardTag(diseaseTags[x]);

        new BukkitRunnable() {
            boolean ovrW = false;

            @Override
            public void run() {
                if(loc.getWorld()==overworld) {
                    p.sendTitle(ChatColor.DARK_GREEN + "The Overworld", ChatColor.GREEN + "" + ChatColor.ITALIC + "Life's Sanctuary", 40, 100, 30);
                    ovrW = true;

                    if (p.getScoreboardTags().size() > 0) {
                        if (!p.getScoreboardTags().contains("overworld"))
                            p.addScoreboardTag("overworld");
                    } else
                        p.addScoreboardTag("overworld");
                } else
                if (p.getScoreboardTags().size() >0)
                    if(p.getScoreboardTags().contains("overworld"))
                        p.removeScoreboardTag("overworld");

                if (!ovrW)
                    if (loc.getWorld()==nether)
                        p.sendTitle(ChatColor.DARK_RED +"The Nether",ChatColor.GOLD +""+ChatColor.ITALIC + "Visceral Hell",40,100,30);
                    else if(loc.getWorld()==end)
                        p.sendTitle(ChatColor.DARK_PURPLE +"The End",ChatColor.BLACK + "" + ChatColor.BOLD + "" +ChatColor.ITALIC + ""  + ChatColor.MAGIC  + "???",40,100,30);

                if(p.getBedSpawnLocation()==null)

                {
                    loc.setY(255);
                    p.teleport(loc);
                    PotionEffectManager.tryApplyPotionEffect(p, new PotionEffect(PotionEffectType.DAMAGE_RESISTANCE, 500, 4, true, true), true);
                    if (p.isFlying())
                        p.setFlying(false);
                }
            }
        }.runTaskLater(this, 1);
    }



    @EventHandler
    public void onPlayerDeath(PlayerDeathEvent e) {
        Player p = e.getEntity();

        if (p.getScoreboardTags().size() > 0) {
            for (String disease : diseaseTags)
                if (p.getScoreboardTags().contains(disease))
                    p.removeScoreboardTag(disease);
        }
    }



    @EventHandler
    public void onPlayerHurt(EntityDamageEvent e) {
        Entity entity = e.getEntity();

        if (entity instanceof Player) {
            Player p = (Player)entity;
            if (p.getScoreboardTags().size() > 0 &&  p.getScoreboardTags().contains("hemophilia") && !p.getScoreboardTags().contains("bleeding"))
                PotionEffectManager.tryApplyCustomPotionEffect(p, CustomPotionEffect.BLEEDING, 246,72000);
        }
    }



    @EventHandler
    public void onPlayerEat(PlayerItemConsumeEvent e) {
        Player p = e.getPlayer();
        ItemStack consumed = e.getItem();

        if (consumed.getType() == Material.MILK_BUCKET) {
            if (p.getScoreboardTags().size() > 0)
                if (p.getScoreboardTags().contains("lactoseInt"))
                    new BukkitRunnable() {
                        @Override
                        public void run() {
                            PotionEffectManager.tryApplyPotionEffect(p, new PotionEffect(PotionEffectType.POISON, 300, 0, false, true, true), true);
                        }
                    }.runTaskLater(this, 1);
        } else if (consumed.getType() == Material.BREAD || consumed.getType() == Material.COOKIE || consumed.getType() == Material.CAKE)
            if (p.getScoreboardTags().size() > 0)
                if (p.getScoreboardTags().contains("celiacs")) {
                    PotionEffectManager.tryApplyPotionEffect(p, new PotionEffect(PotionEffectType.SLOW, 500, 0, false, false, true), true);
                    PotionEffectManager.tryApplyCustomPotionEffect(p, CustomPotionEffect.BLEEDING, 56, 1200);
                }
    }



    @EventHandler
    public void onPortalEvent(EntityPortalEvent e) {
        Entity ent = e.getEntity();
        Location loc2 = e.getTo();

        if (loc2.getWorld() == overworld) {
            if (ent.getScoreboardTags().size() > 0) {
                if (!ent.getScoreboardTags().contains("overworld"))
                    ent.addScoreboardTag("overworld");
            } else
                ent.addScoreboardTag("overworld");
        } else if (ent.getScoreboardTags().size() > 0)
            if (ent.getScoreboardTags().contains("overworld"))
                ent.removeScoreboardTag("overworld");
    }

    @EventHandler
    public void onPlayerPortalEvent(PlayerPortalEvent e) {
        Player p = e.getPlayer();
        Location loc2 = e.getTo();
        boolean ovrW = false;

        if (loc2.getWorld() == overworld) {
            p.sendTitle(ChatColor.DARK_GREEN + "The Overworld", ChatColor.GREEN + "" + ChatColor.ITALIC + "Life's Sanctuary", 80, 100, 30);
            ovrW = true;

            if (p.getScoreboardTags().size() > 0) {
                if (!p.getScoreboardTags().contains("overworld"))
                    p.addScoreboardTag("overworld");
            } else
                p.addScoreboardTag("overworld");
        } else if (p.getScoreboardTags().size() > 0)
            if (p.getScoreboardTags().contains("overworld"))
                p.removeScoreboardTag("overworld");

        if (!ovrW)
            if (loc2.getWorld() == nether)
                p.sendTitle(ChatColor.DARK_RED + "The Nether", ChatColor.GOLD + "" + ChatColor.ITALIC + "Visceral Hell", 40, 100, 30);
            else if (loc2.getWorld() == end)
                p.sendTitle(ChatColor.DARK_PURPLE + "The End", ChatColor.BLACK + "" + ChatColor.BOLD + "" + ChatColor.ITALIC + "" + ChatColor.MAGIC + "???", 40, 100, 30);
    }



    @EventHandler
    public void onBlockBreak(BlockBreakEvent e) {
        Player p = e.getPlayer();
        Location pELoc = p.getEyeLocation();
        ItemStack held = p.getInventory().getItemInMainHand();
        Block bb = e.getBlock();
        World w = bb.getWorld();


        if (p.getGameMode() != GameMode.CREATIVE) {
            if (bb.getType() == Material.NETHERRACK) {
                int chncFactor = 1;
                if (w == nether)
                    chncFactor = 2;

                if (!held.containsEnchantment(Enchantment.SILK_TOUCH) && Math.random() < 0.1 * chncFactor) {
                    w.spawnParticle(Particle.LAVA, bb.getLocation(), (int) Math.ceil(Math.random() * 3), 0.25, 0.25, 0.25, 0.1);

                    if (Math.random() < 0.25 * chncFactor)
                        new BukkitRunnable() {
                            @Override
                            public void run() {
                                bb.setType(Material.FIRE);
                                p.playSound(bb.getLocation(), Sound.ITEM_FIRECHARGE_USE, SoundCategory.BLOCKS, 1, 1);
                            }
                        }.runTaskLater(this, 1);
                    else
                        p.playSound(bb.getLocation(), Sound.BLOCK_LAVA_POP, SoundCategory.BLOCKS, 1, 0.5f);
                }
            }

            if ((held.getType() == Material.WOODEN_SWORD || held.getType() == Material.STONE_SWORD || held.getType() == Material.GOLDEN_SWORD || held.getType() == Material.IRON_SWORD || held.getType() == Material.DIAMOND_SWORD)
                    && (bb.getType() == Material.WHITE_WOOL || bb.getType() == Material.BLACK_WOOL || bb.getType() == Material.BLUE_WOOL ||
                    bb.getType() == Material.BROWN_WOOL || bb.getType() == Material.CYAN_WOOL || bb.getType() == Material.GRAY_WOOL ||
                    bb.getType() == Material.GREEN_WOOL || bb.getType() == Material.LIGHT_BLUE_WOOL || bb.getType() == Material.LIGHT_GRAY_WOOL ||
                    bb.getType() == Material.LIME_WOOL || bb.getType() == Material.MAGENTA_WOOL || bb.getType() == Material.ORANGE_WOOL ||
                    bb.getType() == Material.PINK_WOOL || bb.getType() == Material.PURPLE_WOOL || bb.getType() == Material.RED_WOOL ||
                    bb.getType() == Material.YELLOW_WOOL)) {
                double hmny = Math.random();
                int[] amounts = {2, 3, 4};
                int bonus = 0;
                ItemStack string;
                e.setDropItems(false);

                if (held.hasItemMeta()) {
                    if (held.getItemMeta().hasEnchants())
                        if (held.getItemMeta().hasEnchant(Enchantment.LOOT_BONUS_MOBS))
                            bonus = held.getItemMeta().getEnchantLevel(Enchantment.LOOT_BONUS_MOBS);
                        if (held.getItemMeta() instanceof Damageable) {
                            ItemMeta boboba = held.getItemMeta();
                            ((Damageable)boboba).setDamage(((Damageable)boboba).getDamage() - 1);
                            held.setItemMeta(boboba);
                        }
                }

                for (int n = 0; n < amounts.length; n++)
                    if (amounts[n] + bonus > 4)
                        amounts[n] = 4;
                    else
                        amounts[n] += bonus;

                if (hmny < 0.33) {
                    string = new ItemStack(Material.STRING, amounts[0]);
                    bb.getWorld().dropItemNaturally(bb.getLocation(), string);
                } else if (hmny < 0.66) {
                    string = new ItemStack(Material.STRING, amounts[1]);
                    bb.getWorld().dropItemNaturally(bb.getLocation(), string);
                } else {
                    string = new ItemStack(Material.STRING, amounts[2]);
                    bb.getWorld().dropItemNaturally(bb.getLocation(), string);
                }
            }
        }


        if (held.getType() == Material.WOODEN_HOE || held.getType() == Material.STONE_HOE || held.getType() == Material.IRON_HOE ||
                held.getType() == Material.GOLDEN_HOE || held.getType() == Material.DIAMOND_HOE) {
            boolean silk = false;

            if (held.hasItemMeta())
                if (held.getItemMeta().hasLore())
                    if (held.getItemMeta().getLore().contains(ChatColor.RED + "Curse of Constraint"))
                        silk = true;

            if (!silk) {
                Block[] others = {w.getBlockAt(bb.getX() + 1, bb.getY(), bb.getZ()), w.getBlockAt(bb.getX() + 1, bb.getY(), bb.getZ() + 1),
                        w.getBlockAt(bb.getX(), bb.getY(), bb.getZ() + 1), w.getBlockAt(bb.getX() - 1, bb.getY(), bb.getZ() + 1),
                        w.getBlockAt(bb.getX() - 1, bb.getY(), bb.getZ()), w.getBlockAt(bb.getX() - 1, bb.getY(), bb.getZ() - 1),
                        w.getBlockAt(bb.getX(), bb.getY(), bb.getZ() - 1), w.getBlockAt(bb.getX() + 1, bb.getY(), bb.getZ() - 1)};
                silk = false;

                for (Block b : others)
                    if (b.getType() == Material.WHEAT || b.getType() == Material.BEETROOT || b.getType() == Material.POTATOES ||
                            b.getType() == Material.CARROTS) {
                        if (((Crops) b.getState().getData()).getState() == CropState.RIPE) {
                            if (p.getGameMode() == GameMode.CREATIVE) {
                                BlockData bock = b.getBlockData();

                                b.setType(Material.AIR);
                                b.getWorld().spawnParticle(Particle.BLOCK_CRACK, b.getLocation(), 40, 0.5, 0.5, 0.5, bock);
                                //b.getWorld().playSound(b.getLocation(), Sound.BLOCK_CROP_BREAK, SoundCategory.BLOCKS, 1, 1); TODO
                            } else {
                                b.breakNaturally();
                                //b.getWorld().playSound(b.getLocation(), Sound.BLOCK_CROP_BREAK, SoundCategory.BLOCKS, 1, 1); TODO
                            }

                            silk = true;
                        }
                    } else if (b.getType() == Material.NETHER_WART) {
                        if (((NetherWarts) b.getState().getData()).getState() == NetherWartsState.RIPE) {
                            if (p.getGameMode() == GameMode.CREATIVE) {
                                BlockData bock = b.getBlockData();

                                b.setType(Material.AIR);
                                b.getWorld().spawnParticle(Particle.BLOCK_CRACK, b.getLocation(), 40, 0.5, 0.5, 0.5, bock);
                                //b.getWorld().playSound(b.getLocation(), Sound.BLOCK_NETHERWART_BREAK, SoundCategory.BLOCKS, 1, 1); TODO
                            } else {
                                b.breakNaturally();
                                //b.getWorld().playSound(b.getLocation(), Sound.BLOCK_NETHERWART_BREAK, SoundCategory.BLOCKS, 1, 1); TODO
                            }

                            silk = true;
                        }
                    } else if (b.getType() == Material.BROWN_MUSHROOM || b.getType() == Material.RED_MUSHROOM || b.getType() == Material.GRASS ||
                            b.getType() == Material.TALL_GRASS || b.getType() == Material.SEAGRASS || b.getType() == Material.TALL_SEAGRASS ||
                            b.getType() == Material.KELP_PLANT || b.getType() == Material.SUGAR_CANE || b.getType() == Material.OAK_SAPLING || b.getType() == Material.BIRCH_SAPLING ||
                            b.getType() == Material.SPRUCE_SAPLING || b.getType() == Material.JUNGLE_SAPLING || b.getType() == Material.ACACIA_SAPLING ||
                            b.getType() == Material.DARK_OAK_SAPLING || b.getType() == Material.FERN || b.getType() == Material.LARGE_FERN) {
                        Material mmmm = b.getType();

                        if (p.getGameMode() == GameMode.CREATIVE) {
                            BlockData bock = b.getBlockData();

                            b.setType(Material.AIR);
                            b.getWorld().spawnParticle(Particle.BLOCK_CRACK, b.getLocation(), 40, 0.5, 0.5, 0.5, bock);

                            if (mmmm == Material.SEAGRASS || mmmm == Material.TALL_SEAGRASS || mmmm == Material.KELP_PLANT)
                                b.getWorld().playSound(b.getLocation(), Sound.BLOCK_WET_GRASS_BREAK, SoundCategory.BLOCKS, 1, 1);
                            else
                                b.getWorld().playSound(b.getLocation(), Sound.BLOCK_GRASS_BREAK, SoundCategory.BLOCKS, 1, 1);
                        } else {
                            b.breakNaturally();

                            if (mmmm == Material.SEAGRASS || mmmm == Material.TALL_SEAGRASS || mmmm == Material.KELP_PLANT)
                                b.getWorld().playSound(b.getLocation(), Sound.BLOCK_WET_GRASS_BREAK, SoundCategory.BLOCKS, 1, 1);
                            else
                                b.getWorld().playSound(b.getLocation(), Sound.BLOCK_GRASS_BREAK, SoundCategory.BLOCKS, 1, 1);
                        }

                        silk = true;
                    }

                if (silk) {
                    pELoc.add(p.getLocation().getDirection());
                    w.playSound(bb.getLocation(), Sound.ENTITY_SHEEP_SHEAR, SoundCategory.PLAYERS,1, (float) (0.5 - (Math.random() / 5)));
                    p.getWorld().spawnParticle(Particle.SWEEP_ATTACK, pELoc, 1);
                }
            }
        }
    }



    @EventHandler
    public void onPlayerInteract(PlayerInteractEvent e) {
        Player p = e.getPlayer();
        ItemStack held = e.getItem();
        Action whtDo = e.getAction();

        if ((whtDo == Action.RIGHT_CLICK_AIR || whtDo == Action.RIGHT_CLICK_BLOCK) && held != null)
            if (held.getType() == Material.ENCHANTED_BOOK && held.hasItemMeta()) {
                ItemMeta whyDoYouDoThisToMe = held.getItemMeta();

                if (whyDoYouDoThisToMe.hasLore())
                    if (whyDoYouDoThisToMe.getLore().contains(ChatColor.GOLD + "Right click to perceive")) {
                        List<String> newLore = new ArrayList<>();
                        int totalWeight = 0;

                        for (int x : customEnchantWeights)
                            totalWeight += x;

                        for (int x = 0; x < customEnchants.length; x++) {
                            double weightedChance = customEnchantWeights[x];
                            weightedChance /= (double)totalWeight;

                            if (Math.random() <= weightedChance) {
                                newLore.add(customEnchants[x]);

                                break;
                            }
                        }

                        whyDoYouDoThisToMe.setLore(newLore);
                        held.setItemMeta(whyDoYouDoThisToMe);
                    }
            }
    }



    @EventHandler
    public void onPlayerMove(PlayerMoveEvent e) {
        Player p = e.getPlayer();
        Location loc = e.getFrom();
        Location loc2 = e.getTo();
        World w = p.getWorld();

        if (loc2 != null)
            if (loc.getX() != loc2.getX() || loc.getY() != loc2.getY() || loc.getZ() != loc2.getZ()) {
                Location locy = new Location(loc2.getWorld(), loc2.getX(), loc2.getY() - 0.1, loc2.getZ());
                Block walkie = locy.getBlock();

                if (!p.isSneaking())
                    if (walkie.getType() == Material.SOUL_SAND) {
                        w.playSound(loc2, Sound.ENTITY_GHAST_HURT, SoundCategory.AMBIENT, 1.5f, 1);
                        PotionEffectManager.tryApplyPotionEffect(p, new PotionEffect(PotionEffectType.SLOW, 20, 5, false, false), true);
                    }
            }
    }



    @SuppressWarnings("unchecked")
    @Override
    public void onDisable() {
        cfgm.savePlayers();

        try {
            Field byKeyField = Enchantment.class.getDeclaredField("byKey");
            Field byNameField = Enchantment.class.getDeclaredField("byName");

            byKeyField.setAccessible(true);
            byNameField.setAccessible(true);

            HashMap<NamespacedKey, Enchantment> byKey = (HashMap<NamespacedKey, Enchantment>) byKeyField.get(null);
            HashMap<String, Enchantment> byName = (HashMap<String, Enchantment>) byNameField.get(null);

            if (byKey.containsKey(glow.getKey()))
                byKey.remove(glow.getKey());

            if (byName.containsKey(glow.getName()))
                byName.remove(glow.getName());
        } catch (Exception ignored) {}

        if (sunOverrideEnabled)
            overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, true);
    }




    private static int conv(String a) {
        return Integer.parseInt(LocalDateTime.now().format(DateTimeFormatter.ofPattern(a)));
    }

    static Gang getInstance() {
        return instance;
    }

    private void loadConfigManager() {
        cfgm = new ConfigManager();
        cfgm.setup();
    }

    static void heal(Player p, double healAmount) {
        double maxHealth = p.getAttribute(Attribute.GENERIC_MAX_HEALTH).getValue();
        double hp2max = maxHealth - p.getHealth();

        if (p.getHealth() == maxHealth)
            return;

        if (healAmount >= hp2max)
            p.setHealth(maxHealth);
        else
            p.setHealth(p.getHealth() + healAmount);
    }
    static void heal(LivingEntity e, double healAmount) {
        double maxHealth = e.getAttribute(Attribute.GENERIC_MAX_HEALTH).getValue();
        double hp2max = maxHealth - e.getHealth();

        if (e.getHealth() == maxHealth)
            return;

        if (healAmount >= hp2max)
            e.setHealth(maxHealth);
        else
            e.setHealth(e.getHealth() + healAmount);
    }
}
