package epiphany.commapickle.Proxies;

import epiphany.commapickle.Commands.CPGlobalCommand;
import net.minecraftforge.client.ClientCommandHandler;
import net.minecraftforge.fml.common.event.FMLInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPostInitializationEvent;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

public class ClientProxy implements IProxy {
    @Override
    public void preInit(FMLPreInitializationEvent e) {

    }

    @Override
    public void init(FMLInitializationEvent e) {
        ClientCommandHandler.instance.registerCommand(new CPGlobalCommand());
    }

    @Override
    public void postInit(FMLPostInitializationEvent e) {

    }
}
