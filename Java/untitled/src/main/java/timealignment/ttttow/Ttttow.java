package timealignment.ttttow;

import org.bukkit.*;
import org.bukkit.entity.*;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.AsyncPlayerChatEvent;
import org.bukkit.event.player.PlayerBedEnterEvent;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.scheduler.BukkitRunnable;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public final class Ttttow extends JavaPlugin implements Listener {
    private static final String[] morningMessage = {"<The_Sun> Good morning!", "<The_Sun> Hey Vsauce, Micheal here!", "<The_Sun> Hi sisters!!", "<The_Sun> Where are your fingers?", "<The_Sun> Good morning Bella!"};
    private static final String[] sunsetMessage = {"<The_Sun> Good night!", "<The_Sun> RRREEEEEEEEEEEEEE!!!11!", "* The_Moon sighs"};
    private static boolean tOvrD = false; // Tells whether or not the aligned time cycle is being overridden
    private static boolean tOvrD2 = false; // Tells whether the overridden time cycle is at day
    private static boolean tOvrD3 = false; // Tells whether the overridden time cycle is at night
    private static World overworld;
    private static Ttttow instance;

    @Override
    public void onEnable() {
        getServer().getPluginManager().registerEvents(this, this);
        overworld = Bukkit.getWorlds().get(0);
        overworld.setGameRule(GameRule.DO_DAYLIGHT_CYCLE, false);
        instance = this;

        new BukkitRunnable() {
            boolean sunMessage = false;
            long currentTime;
            int S;

            @Override
            public void run() {
                S = (int) Math.floor(((conv("HH") * 1000) - 6000) + (conv("mm") * 16.7)); // Converts real time to minecraft time
                currentTime = overworld.getTime();
                int chooseName;

                if (S >= 22900 && S <= 23100) {
                    if (!sunMessage) { // Only makes the message happen once per sunrise
                        chooseName = (int) Math.floor(Math.random() * (morningMessage.length - 1));

                        getServer().getOnlinePlayers().forEach(p -> p.sendMessage(ChatColor.YELLOW + "The_Sun joined the game"));
                        new BukkitRunnable() {
                            @Override
                            public void run() {
                                getServer().getOnlinePlayers().forEach(p -> p.sendMessage(sunsetMessage[chooseName]));
                            }
                        }.runTaskLater(instance, 20);
                        sunMessage = true;
                    }
                } else if (S >= 13800 && S <= 14000) {
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
            }
        }.runTaskTimer(this, 20, 200);

    }

    @EventHandler
    public void chatCheck(AsyncPlayerChatEvent e) {
        String message = e.getMessage();
        Player p = e.getPlayer();

        if (message.toLowerCase().contains("bad sun") && !tOvrD && p.getWorld() == overworld) {
            if (p.getLocation().getY() >= 256 && overworld.getTime() >= 0 && overworld.getTime() <= 12000 && p.getLocation().getPitch() == -90) {
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
        }
    }

    @EventHandler
    public void woke(PlayerBedEnterEvent e) {
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

    @Override
    public void onDisable() {
    }

    private int conv(String a) {
        return Integer.parseInt(LocalDateTime.now().format(DateTimeFormatter.ofPattern(a))); // Grabs time from real life, specifies what type of time with strings
    }
}
