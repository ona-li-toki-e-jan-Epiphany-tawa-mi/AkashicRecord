package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonSyntaxException;
import org.bukkit.ChatColor;
import org.bukkit.command.*;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.MainBoi;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;

/**
 * Used to create and send messages that are localized to players.
 * Localizations files are stored in MainBoi/lang/
 */
public class LocalizedMessages implements CommandExecutor, TabCompleter {
    private static final HashMap<String, HashMap<String, String>> localizedMessages = new HashMap<>();
    private static String defaultLanguage;

    public static void onEnable(File dataFolder, FileConfiguration configurationFile) {
        JsonParser jsonParser = new JsonParser();
        File languageFolder = new File(dataFolder.getAbsolutePath() + "/lang");

        if (!languageFolder.exists())
            languageFolder.mkdir();

        File[] languageFiles = languageFolder.listFiles();

        // Gets localization files and loads their data.
        if (languageFiles != null && languageFiles.length > 0)
            for (File file : languageFiles) {
                String fileName = file.getName();

                if (fileName.substring(fileName.lastIndexOf(".") + 1).equals("json"))
                    try {
                        JsonObject fileData = jsonParser.parse(new FileReader(file)).getAsJsonObject();
                        JsonElement languageCode = fileData.get("language.code");

                        if (languageCode != null) {
                            HashMap<String, String> keyMessageSet = new HashMap<>();

                            // Loads localization data into a HashMap.
                            for (Map.Entry<String, JsonElement> data : fileData.entrySet())
                                keyMessageSet.put(data.getKey(), data.getValue().getAsString());

                            // Loads the localization data HashMap into the localizedMessages HashMap.
                            localizedMessages.put(languageCode.getAsString(), keyMessageSet);
                        }

                    } catch (FileNotFoundException | JsonSyntaxException exception) {
                        exception.printStackTrace();
                    }
            }

        // Sets the language to default to if the player does not have a set and known locale.
        String givenLanguage = configurationFile.getString("lang.default");

        if (givenLanguage == null) {
            configurationFile.set("lang.default", "en_us");
            MainBoi.getInstance().saveConfig();

            givenLanguage = "en_us";
        }

        defaultLanguage = givenLanguage;

        if (localizedMessages.size() == 0)
            MainBoi.getInstance().getLogger().severe("There are no valid localization files loaded into the plugin. Names and messages will be nonsensical! Please place a valid localization file into MainBoi/lang/");

        // Registers command listeners.
        PluginCommand capabilitiesCommand = Objects.requireNonNull(MainBoi.getInstance().getCommand("setlocale"));
        LocalizedMessages localizedMessages = new LocalizedMessages();
        capabilitiesCommand.setExecutor(localizedMessages);
        capabilitiesCommand.setTabCompleter(localizedMessages);
    }



    /**
     * Gets the code of the first language that the localized message is found in.
     *
     * @param localizedMessage The message to look for. (e.x.: Weird Thing, ijo nasa)
     *
     * @return The language code of the first language that contains localizedMessage, or null, if it isn't found.
     */
    public static String getLanguageCode(String localizedMessage) {
        for (Map.Entry<String, HashMap<String, String>> messageSetEntries : localizedMessages.entrySet())
            if (messageSetEntries.getValue().containsValue(localizedMessage))
                return messageSetEntries.getKey();

        return null;
    }

    /**
     * Gets the code of the first language that has the given language name.
     *
     * @param languageName The name of the language. (e.x.: English, toki pona, Русский, 日本語).
     *
     * @return The language code of the first language that has the given language name, or null, if it isn't found.
     */
    public static String getLanguageFromName(String languageName) {
        for (String languageCode : localizedMessages.keySet()) {
            String randomLanguageName = getMessage(languageCode, "language.name");

            if (randomLanguageName != null && randomLanguageName.equals(languageName))
                return languageCode;
        }

        return null;
    }

    /**
     * @return The default language code set in the configuration file.
     */
    public static String getDefaultLanguage() {
        return defaultLanguage;
    }

    /**
     * @return A list of the loaded languages' language codes.
     */
    public static Set<String> getLanguageCodes() {
        return localizedMessages.keySet();
    }


    /**
     * Returns a player's language code. defaults to defaultLanguage.
     * Language data is stored in tags.
     *
     * @param player The player to get the code from.
     *
     * @return The player's language code.
     */
    public static String getPlayerLanguage(Player player) {
        for (String tag : player.getScoreboardTags())
            if (tag.startsWith("locale=")) {
                String locale = tag.split("=", 2)[1];

                if (localizedMessages.containsKey(locale))
                    return locale;

                break;
            }

        return defaultLanguage;
    }

    /**
     * Sets a player's language.
     * Language data is stored in tags.
     *
     * @param player The player to set the language of.
     * @param languageCode The language to set it to. (e.x.: en_us, tok, ja, ru)
     *
     * @return Whether or not the language was set.
     */
    public static boolean setPlayerLanguage(Player player, String languageCode) {
        if (localizedMessages.containsKey(languageCode)) {
            String languageTag = "locale=" + languageCode;

            for (String tag : player.getScoreboardTags())
                if (tag.startsWith("locale="))
                    if (!tag.equals(languageTag)) {
                        player.removeScoreboardTag(tag);
                        break;

                    } else
                        return false;

            player.addScoreboardTag(languageTag);
            return true;
        }

        return false;
    }


    /**
     * Gets the key of the first key-value pair found that contains localizedMessage as a value.
     *
     * @param localizedMessage The message to look for. (e.x.: Weird Thing, ijo nasa).
     * @param languageCode The code for the language to use. (e.x.: en_us, tok, ja, ru)
     *
     * @return The key of the first key-value pair found that contains localizedMessage as a value, or null, if it isn't found.
     */
    public static String getKey(String languageCode, String localizedMessage) {
        if (localizedMessages.containsKey(languageCode))
            for (Map.Entry<String, String> messageEntries : localizedMessages.get(languageCode).entrySet())
                if (messageEntries.getValue().equals(localizedMessage))
                    return messageEntries.getKey();

        return null;
    }

    /**
     * Gets the key of the first key-value pair of the any language found that contains localizedMessage as a value.
     *
     * @param localizedMessage The message to look for. (e.x.: Weird Thing, ijo nasa)
     *
     * @return The key of the first key-value pair of any language found that contains localizedMessage as a value, or null, if it isn't found.
     */
    public static String getKey(String localizedMessage) {
        for (HashMap<String, String> messageSetEntries : localizedMessages.values())
            for (Map.Entry<String, String> messageEntries : messageSetEntries.entrySet())
                if (messageEntries.getValue().equals(localizedMessage))
                    return messageEntries.getKey();

        return null;
    }


    /**
     * Gets a message in its localized form using its language and its base form.
     *
     * @param languageCode The code for the language to use. (e.x.: en_us, tok, ja, ru)
     * @param baseMessage The base form of the message. (e.x.: entity.weird_thing.name)
     *
     * @return The localized form of this message. Will return the base message if no localized form is found.
     */
    public static String getMessage(String languageCode, String baseMessage) {
        if (localizedMessages.containsKey(languageCode))
            return localizedMessages.get(languageCode).get(baseMessage);

        return baseMessage;
    }

    /**
     * Gets the localized form of a message for an entity.
     * For players it returns the localization of that message in their locale.
     * For anything else it returns the localization of that message in the default locale.
     *
     * @param entity The entity to get the message for.
     * @param baseMessage The base from of the message. (e.x.: entity.weird_thing.name)
     *
     * @return The localized form of the message in the entity's locale, or the default, if its locale cannot be found.
     */
    public static String getMessageFor(Entity entity, String baseMessage) {
        String languageCode;

        if (entity instanceof Player) {
            languageCode = getPlayerLanguage((Player) entity);

        } else
            languageCode = defaultLanguage;

        return getMessage(languageCode, baseMessage);
    }

    /**
     * Gets the localized form of a message for a command sender.
     * For players it returns the localization of that message in their locale.
     * For anything else it returns the localization of that message in the default locale.
     *
     * @param sender The CommandSender to get the message for.
     * @param baseMessage The base from of the message. (e.x.: entity.weird_thing.name)
     *
     * @return The localized form of the message in the sender's locale, or the default, if its locale cannot be found.
     */
    public static String getMessageFor(CommandSender sender, String baseMessage) {
        String languageCode;

        if (sender instanceof Player) {
            languageCode = getPlayerLanguage((Player) sender);

        } else
            languageCode = defaultLanguage;

        return getMessage(languageCode, baseMessage);
    }



    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        if (sender instanceof Player && args.length >= 1) {
            Player player = (Player) sender;
            String selectedLanguage;

            // Gets the language chosen by the player.
            if (args.length > 1) {
                StringBuilder bigArgument = new StringBuilder();
                int lastIndex = args.length - 1;

                for (int i = 0; i < args.length; i++) {
                    bigArgument.append(args[i]);

                    if (i != lastIndex)
                        bigArgument.append(" ");
                }

                selectedLanguage = bigArgument.toString();

            } else
                selectedLanguage = args[0];

            String languageCode = getLanguageFromName(selectedLanguage);

            // Sets the chosen language.
            if (languageCode != null) {
                boolean success = setPlayerLanguage(player, languageCode);

                if (success) {
                    player.sendMessage(getMessage(languageCode, "command.setlocale.success"));

                } else
                    player.sendMessage(ChatColor.RED + getMessage(languageCode, "command.setlocale.already_set"));

            } else
                player.sendMessage(ChatColor.RED + getMessageFor(player, "command.setlocale.not_found")
                        .replaceAll("%s", selectedLanguage));

            return true;

        } else
            sender.sendMessage(ChatColor.RED + getMessage(defaultLanguage, "command.setlocale.only_players"));

        return false;
    }

    @Override
    public List<String> onTabComplete(CommandSender sender, Command command, String alias, String[] args) {
        if (args.length == 1) {
            List<String> languageNames = new ArrayList<>();

            for (String languageCode : localizedMessages.keySet()) {
                String languageName = getMessage(languageCode, "language.name");

                if (languageName != null)
                    languageNames.add(languageName);
            }

            return languageNames;
        }

        return new ArrayList<>();
    }
}
