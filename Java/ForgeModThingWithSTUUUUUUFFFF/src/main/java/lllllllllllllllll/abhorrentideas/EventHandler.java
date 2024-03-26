package lllllllllllllllll.abhorrentideas;

import lllllllllllllllll.abhorrentideas.badideas.OneHitSpike;
import net.minecraftforge.common.MinecraftForge;

/**
 * Handles stuff related to events.
 */
public class EventHandler {
    static void registerEvents() {
        MinecraftForge.EVENT_BUS.register(new OneHitSpike());
    }
}
