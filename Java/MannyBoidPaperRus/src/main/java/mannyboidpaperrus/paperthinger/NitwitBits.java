package mannyboidpaperrus.paperthinger;

import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;

class NitwitBits {
    /**
     * A command that calls itself...
     * A command that calls itself...
     * A command that calls itself...
     * A command that calls itself...
     * A command that calls itself...
     * A command that ca- you get the idea.
     */
    static boolean onCommandRecursion(CommandSender sender, Command command, String label, String[] args) {
        return Bukkit.dispatchCommand(sender, "recursion");
    }
}
