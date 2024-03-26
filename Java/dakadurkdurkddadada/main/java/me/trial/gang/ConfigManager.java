package me.trial.gang;

import org.bukkit.Bukkit;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;

public class ConfigManager {
    private Gang gang = Gang.getPlugin(Gang.class);

    private FileConfiguration playerCfg;
    private File playerFiles;

    void setup() {
        if (!gang.getDataFolder().exists())
            gang.getDataFolder().mkdir();

        playerFiles = new File(gang.getDataFolder(), "players.yml");

        if (!playerFiles.exists())
            try {
                playerFiles.createNewFile();
            } catch (IOException e) {
                Bukkit.getLogger().info("File creation failure");
            }

        playerCfg = YamlConfiguration.loadConfiguration(playerFiles);
    }

    public FileConfiguration getPlayers() {
        return playerCfg;
    }


     void savePlayers() {
        try {
            playerCfg.save(playerFiles);
        } catch (IOException e) {
            Bukkit.getLogger().info("File save failure");
        }
    }

    public void reloadPlayers() {
        playerCfg = YamlConfiguration.loadConfiguration(playerFiles);
    }
}
