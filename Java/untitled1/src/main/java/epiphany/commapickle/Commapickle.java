package epiphany.commapickle;

import epiphany.commapickle.Modules.AssistedMovement;
import epiphany.commapickle.Proxies.IProxy;

import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.SidedProxy;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPostInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.TickEvent;

@Mod(
        modid = Commapickle.MOD_ID,
        name = Commapickle.MOD_NAME,
        version = Commapickle.MOD_VERSION
)
public class Commapickle {
    static final String MOD_ID = "commapickle";
    static final String MOD_NAME = "commapickle";
    static final String MOD_VERSION = "commapickle";
    private static final String CLIENT_PROXY_PATH = "epiphany.commapickle.Proxies.ClientProxy";
    private static final String SERVER_PROXY_PATH = "epiphany.commapickle.Proxies.ServerProxy";

    private static long prevTime = 0; // Used for getting delta time

    @SidedProxy(clientSide = CLIENT_PROXY_PATH, serverSide = SERVER_PROXY_PATH)
    public static IProxy proxy;

    @Mod.Instance(MOD_ID)
    private static Commapickle INSTANCE;

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        proxy.preInit(event);
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        MinecraftForge.EVENT_BUS.register(this);

        proxy.init(event);
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        proxy.postInit(event);

        prevTime = System.nanoTime();
    }

    @SubscribeEvent
    public void mainLoop(TickEvent.ClientTickEvent event) {
        TickEvent.Phase phase = event.phase;

        if (phase == TickEvent.Phase.END) {
            double deltaTime = (System.nanoTime() - prevTime) / 1_000_000_000.0;
            prevTime = System.nanoTime();

            Minecraft client = Minecraft.getMinecraft();
            WorldClient world = client.world;

            if (world != null) {
                AssistedMovement.clientPeriodic(client, deltaTime);
            }

            //CPacketPlayer.PositionRotation cpp = new CPacketPlayer.PositionRotation();
        }
    }
}
