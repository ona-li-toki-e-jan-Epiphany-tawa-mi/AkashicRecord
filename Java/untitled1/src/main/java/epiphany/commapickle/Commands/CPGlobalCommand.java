package epiphany.commapickle.Commands;

import epiphany.commapickle.Modules.AssistedMovement;
import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.client.IClientCommand;

import java.util.Collections;
import java.util.List;

public class CPGlobalCommand extends CommandBase implements IClientCommand {
    @Override
    public boolean allowUsageWithoutPrefix(ICommandSender sender, String message) {
        return false;
    }

    @Override
    public String getName() {
        return "commapickle";
    }

    @Override
    public String getUsage(ICommandSender sender) {
        return "A global command to be used by all modules for enabling/disabling, or whatever they want I guess.";
    }

    @Override
    public void execute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
        if (args.length <= 0)
            throw new CommandException("commands.commapickle.missingArguments");
        else {
            if (args[0].equalsIgnoreCase("assistedMovement") || args[0].equalsIgnoreCase("aM"))
                AssistedMovement.moduleCommandExecute(server, sender, args);
            else
                throw new CommandException("commands.commapickle.missingArguments");
        }
    }

    @Override
    public List<String> getAliases() {
        return Collections.singletonList("cp");
    }

    @Override
    public int getRequiredPermissionLevel() {
        return 0;
    }
}
