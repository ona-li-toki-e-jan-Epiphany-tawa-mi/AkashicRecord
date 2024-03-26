package maalanasapimeja.mlnspmj;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.DamageSource;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.TickEvent.Phase;
import net.minecraftforge.event.entity.player.PlayerSleepInBedEvent;
import net.minecraftforge.event.entity.player.PlayerWakeUpEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.Event.Result;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;

import java.util.ArrayList;

/**
 * ijo lape lon MANP.
 */
public class Lape {
    private static final ArrayList<PlayerEntity> pokiPiJanLape = new ArrayList<>();

    /**
     * jan li lape lon MANP la ni li pana e ona lon pokiPiJanLape.
     */
    @SubscribeEvent(priority = EventPriority.LOWEST)
    public static void janLiLukinLapeLonSupaLonMaAla(PlayerSleepInBedEvent kamaPiJanLiLapeLonSupa) {
        PlayerEntity jan = kamaPiJanLiLapeLonSupa.getPlayer();
        Event.Result ijoKama = kamaPiJanLiLapeLonSupa.getResult();

        if ((ijoKama.equals(Result.DEFAULT) || ijoKama.equals(Result.ALLOW)) && !jan.isCreative()
                && jan.getEntityWorld().getDimensionKey().getLocation().toString().equals("mlnspmj:ma_ala_nasa_pimeja")) {
            MinecraftServer maPiJanMute = jan.getServer();

            if (maPiJanMute != null)
                pokiPiJanLape.add(jan);
        }
    }

    /**
     * tenpo Tick li kama la jan li lape suli lon MANP la ni li pakala e ona.
     */
    @SubscribeEvent
    public static void tenpo_TickLiKama(TickEvent.ServerTickEvent kamaPiTenpo_Tick) {
        if (kamaPiTenpo_Tick.phase == Phase.START && !pokiPiJanLape.isEmpty())
            for (PlayerEntity jan : pokiPiJanLape)
                if (!jan.isSleeping() || jan.isCreative() || !jan.getEntityWorld().getDimensionKey().getLocation().toString().equals("mlnspmj:ma_ala_nasa_pimeja")) {
                    jan.remove();

                } else if (jan.isPlayerFullyAsleep()) {
                    jan.attackEntityFrom(DamageSource.WITHER, 1);
                }
    }

    @SubscribeEvent
    public static void janLiPiniLapeTanPakalaPiPimejaLape(PlayerWakeUpEvent kamaPiJanLiPiniLape) {
        PlayerEntity jan = kamaPiJanLiPiniLape.getPlayer();

        if (kamaPiJanLiPiniLape.wakeImmediately() && !jan.getEntityWorld().isRemote && pokiPiJanLape.contains(jan)) {}
            //kamaPiJanLiPiniLape.setCanceled(true);
    }
}
