package maalanasapimeja.mlnspmj;

import net.minecraft.block.Block;
import net.minecraft.block.Blocks;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.InterModComms;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.event.lifecycle.InterModEnqueueEvent;
import net.minecraftforge.fml.event.lifecycle.InterModProcessEvent;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.stream.Collectors;

// TODO ma ala pi MANP. ona li jo e ijo lili kin.

// TODO jan li lape lon supa lape la ona li moli.
// TODO jan li kepeken e leko lape pi ma seli la ala li kama.
// TODO jan li ken ala kepeken e ilo pi alasa ma. ona li wile nasa mute.
// TODO jan li lon MANP la ona li kama pakala lon tenpo. ona li lape lon MANP la pakala li kama mute mute.

// TODO nanpa CoordinateScale pi MANP li wile ante lon tenpo.
// TODO sinpin suli suli pi kiwen pimeja li lon.
// TODO lupa suli tawa MANP li lon ma jan.

// TODO monsuta pi oko kalama. ona li lukin kepeken kalama.
// TODO monsuta jasima. ona li pali e ijo. mi sona ala e ona.
// TODO monsuta walo pi ilo utala pi palisa kon li kama pimeja lon MANP.

@Mod("mlnspmj")
public class MaAlaNasaPimeja {
    public static final Logger iloToki = LogManager.getLogger();

    public MaAlaNasaPimeja() {
        // Register the setup method for modloading
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
        // Register the enqueueIMC method for modloading
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::enqueueIMC);
        // Register the processIMC method for modloading
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::processIMC);
        // Register the doClientStuff method for modloading
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::doClientStuff);

        MinecraftForge.EVENT_BUS.register(Lape.class);

        // Register ourselves for server and other game events we are interested in
        MinecraftForge.EVENT_BUS.register(this);
    }

    private void setup(final FMLCommonSetupEvent event) {
        // some preinit code
        iloToki.info("toki tan ilo PREINIT");
        iloToki.info("leko ma >> {}", Blocks.DIRT.getRegistryName());
    }

    private void doClientStuff(final FMLClientSetupEvent event) {
        // do something that can only be done on the client
        iloToki.info("li pini kama jo e ilo lawa musi {}", event.getMinecraftSupplier().get().gameSettings);
    }

    private void enqueueIMC(final InterModEnqueueEvent event) {
        // some example code to dispatch IMC to another mod
        InterModComms.sendTo("mlnspmj", "helloworld", () -> {
            iloToki.info("toki tan ilo MDK");
            return "ma o, toki";
        });
    }

    private void processIMC(final InterModProcessEvent event) {
        // some example code to receive and process InterModComms from other mods
        iloToki.info("Got IMC {}", event.getIMCStream().
                map(m -> m.getMessageSupplier().get()).
                collect(Collectors.toList()));
    }

    // You can use SubscribeEvent and let the Event Bus discover methods to call
    @SubscribeEvent
    public void onServerStarting(FMLServerStartingEvent event) {
        // do something when the server starts
        iloToki.info("HELLO from server starting");
    }

    // You can use EventBusSubscriber to automatically subscribe events on the contained class (this is subscribing to the MOD
    // Event bus for receiving Registry Events)
    @Mod.EventBusSubscriber(bus = Mod.EventBusSubscriber.Bus.MOD)
    public static class RegistryEvents {
        @SubscribeEvent
        public static void onBlocksRegistry(final RegistryEvent.Register<Block> blockRegistryEvent) {
            // register a new block here
            iloToki.info("HELLO from Register Block");
        }
    }
}
