package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

// TODO Have names put in buildRandomDeathMessage() carry formatting (selector arguments.)

/**
 * A series of lists used to cleanly pick death messages.
 */
public final class DeathMessageLists {
    private static final Random random = new Random();

    // List of death messages for freeze effect execution and death to ice bombs.
    public final static ArrayList<String> FREEZE_DEATH_MESSAGES = new ArrayList<>(Arrays.asList(
            "%s was straight iced", "%s got a brain freeze", "%s was shattered into a million pieces", "%s forgot their blanket", "%s couldn't handle the cold",
            "%s forgot to close the freezer", "%s got the cold shoulder", "%s was frozen in time", "%s was turned into a popsicle, funniest shit I've ever seen"
    ));

    /**
     * Constructs a death message from a list of strings.
     * Replaces all occurrences of "%s" with victimName.
     *
     * @param messageList The list of messages to pull from.
     * @param victimName The name of the victim.
     *
     * @return The constructed deathMessage.
     */
    public static String buildRandomDeathMessage(ArrayList<String> messageList, String victimName) {
        String message = messageList.get((int) (messageList.size() * random.nextDouble()));
        return message.replaceAll("%s", victimName);
    }

    /**
     * Constructs a death message from a list of strings.
     * Replaces all occurrences of "%s1" with victimName.
     * Replaces all occurrences of "%s2" with attackerName.
     *
     * @param messageList The list of messages to pull from.
     * @param victimName The name of the victim.
     * @param attackerName The name of the attacker.
     *
     * @return The constructed deathMessage.
     */
    public static String buildRandomDeathMessage(ArrayList<String> messageList, String victimName, String attackerName) {
        String message = messageList.get((int) (messageList.size() * random.nextDouble()));
        return message.replaceAll("%s1", victimName).replaceAll("%s2", attackerName);
    }
}
