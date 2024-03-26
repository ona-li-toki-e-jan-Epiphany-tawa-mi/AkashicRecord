package epiphany.commapickle.Modules;

import net.minecraft.block.properties.PropertyBool;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.EntityPlayerSP;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.command.CommandBase;
import net.minecraft.command.CommandException;
import net.minecraft.command.ICommandSender;
import net.minecraft.entity.MoverType;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.MovementInput;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TextComponentString;

public class AssistedMovement {
    private static boolean moduleEnabled = true;
    private static boolean stepAssistEnabled = true;
    private static boolean sanicSpeedEnabled = false;
    private static float speedFactor = 2;

    // Add climbing!
    // Speedy Sprint

    public static void moduleCommandExecute(MinecraftServer server, ICommandSender sender, String[] args) throws CommandException {
        if (args.length < 2)
            throw new CommandException("commands.commapickle.assistedMovement.missingArguments");
        else {
            if (args[1].equalsIgnoreCase("enable") || args[1].equalsIgnoreCase("e")) {

                moduleEnabled = true;
                sender.sendMessage(new TextComponentString("Assisted Movement Enabled"));

            } else if (args[1].equalsIgnoreCase("disable") || args[1].equalsIgnoreCase("d")) {

                moduleEnabled = false;
                sender.sendMessage(new TextComponentString("Assisted Movement Disabled"));

            } else if (args[1].equalsIgnoreCase("stepAssist") || args[1].equalsIgnoreCase("sA")) {

                if (args.length == 2)
                    throw new CommandException("commands.commapickle.assistedMovement.stepAssist.missingArguments");
                else if (args[2].equalsIgnoreCase("enable") || args[2].equalsIgnoreCase("e")) {
                    stepAssistEnabled = true;
                    sender.sendMessage(new TextComponentString("Step Assist Enabled"));
                } else if (args[2].equalsIgnoreCase("disable") || args[2].equalsIgnoreCase("d")) {
                    stepAssistEnabled = false;
                    sender.sendMessage(new TextComponentString("Step Assist Disabled"));
                } else
                    throw new CommandException("commands.commapickle.assistedMovement.stepAssist.missingArguments");

            } else if (args[1].equalsIgnoreCase("sanicSpeed") || args[1].equalsIgnoreCase("sS")) {

                if (args.length == 2)
                    throw new CommandException("commands.commapickle.assistedMovement.sanicSpeed.missingArguments");
                else if (args[2].equalsIgnoreCase("enable") || args[2].equalsIgnoreCase("e")) {
                    sanicSpeedEnabled = true;
                    sender.sendMessage(new TextComponentString("Sanic Speed Enabled"));
                } else if (args[2].equalsIgnoreCase("disable") || args[2].equalsIgnoreCase("d")) {
                    sanicSpeedEnabled = false;
                    sender.sendMessage(new TextComponentString("Sanic Speed Disabled"));
                } else if (args[2].equalsIgnoreCase("set") || args[2].equalsIgnoreCase("s")) {
                    if (args.length == 3)
                        throw new CommandException("commands.commapickle.assistedMovement.sanicSpeed.missingArguments");
                    else {
                        speedFactor = (float) CommandBase.parseDouble(args[3], 0);
                        sender.sendMessage(new TextComponentString("Sanic Speed Factor set to " + speedFactor));
                    }
                } else
                    throw new CommandException("commands.commapickle.assistedMovement.sanicSpeed.missingArguments");

            } else
                throw new CommandException("commands.commapickle.assistedMovement.missingArguments");
        }
    }

    public static void clientPeriodic(Minecraft client, double deltaTime) {
        WorldClient world = client.world;
        EntityPlayerSP player = client.player;

        if (moduleEnabled) {
            // Perfect this to be mathematically accurate
            if (stepAssistEnabled && !player.isSneaking() && !player.isRiding() && !player.isAirBorne &&
                    !isValidBlock(world, player.getPosition().add(0, 2, 0))) {
                BlockPos northPos = new BlockPos(player.posX + 0.1, player.posY + 0.55, player.posZ);
                BlockPos westPos = new BlockPos(player.posX, player.posY + 0.55, player.posZ + 0.1);
                BlockPos southPos = new BlockPos(player.posX - 0.1, player.posY + 0.55, player.posZ);
                BlockPos eastPos = new BlockPos(player.posX, player.posY + 0.55, player.posZ - 0.1);
                boolean wentUp = false;

                if (isValidBlock(world, northPos) && !isValidBlock(world, northPos.add(0, 1, 0)) && !isValidBlock(world, northPos.add(0, 2, 0))) {
                    player.move(MoverType.PLAYER, 0, 1, 0);
                    wentUp = true;

                    player.move(MoverType.PLAYER, 0.1, 0, 0);
                }
                if (isValidBlock(world, westPos) && !isValidBlock(world, westPos.add(0, 1, 0)) && !isValidBlock(world, westPos.add(0, 2, 0))) {
                    if (!wentUp) {
                        player.move(MoverType.PLAYER, 0, 1, 0);
                        wentUp = true;
                    }

                    player.move(MoverType.PLAYER, 0, 0, 0.1);
                }
                if (isValidBlock(world, southPos) && !isValidBlock(world, southPos.add(0, 1, 0)) && !isValidBlock(world, southPos.add(0, 2, 0))) {
                    if (!wentUp) {
                        player.move(MoverType.PLAYER, 0, 1, 0);
                        wentUp = true;
                    }

                    player.move(MoverType.PLAYER, -0.2, 0, 0);
                }
                if (isValidBlock(world, eastPos) && !isValidBlock(world, eastPos.add(0, 1, 0)) && !isValidBlock(world, eastPos.add(0, 2, 0))) {
                    if (!wentUp)
                        player.move(MoverType.PLAYER, 0, 1, 0);

                    player.move(MoverType.PLAYER, 0, 0, -0.2);
                }
            }

            if (sanicSpeedEnabled && !player.isRiding()) {
                MovementInput playerInput = player.movementInput;

                if (playerInput.forwardKeyDown && !playerInput.backKeyDown)
                    player.travel(0, 0, speedFactor *(float) deltaTime);
                else if (!playerInput.forwardKeyDown && playerInput.backKeyDown)
                    player.travel(0, 0, -speedFactor *(float) deltaTime);

                if (playerInput.leftKeyDown && !playerInput.rightKeyDown)
                    player.travel(speedFactor *(float) deltaTime, 0, 0);
                else if (!playerInput.leftKeyDown && playerInput.rightKeyDown)
                    player.travel(-speedFactor *(float) deltaTime, 0, 0);

                if (playerInput.jump && !playerInput.sneak)
                    player.travel(0, speedFactor *(float) deltaTime * 4, 0);
                else if (!playerInput.jump && playerInput.sneak)
                    player.travel(0, -speedFactor *(float) deltaTime, 0);
            }
        }
    }

    // Perfect this to be even more general
    private static boolean isValidBlock(WorldClient world, BlockPos pos) {
        if (!world.isBlockLoaded(pos, false) || world.isAirBlock(pos))
            return false;

        String blockName = world.getBlockState(pos).getBlock().toString();
        if (blockName.equals("Block{minecraft:chest}") || blockName.equals("Block{minecraft:enchanting_table}") ||
                blockName.equals("Block{minecraft:end_portal_frame}") || blockName.equals("Block{minecraft:ender_chest}"))
            return true;
        if (blockName.equals("Block{minecraft:piston}") || blockName.equals("Block{minecraft:sticky_piston}")) {
            IBlockState blockState = world.getBlockState(pos);

            return !blockState.getValue(PropertyBool.create("extended"));
        }

        return world.isBlockNormalCube(pos, false) || world.isBlockFullCube(pos);
    }
}
