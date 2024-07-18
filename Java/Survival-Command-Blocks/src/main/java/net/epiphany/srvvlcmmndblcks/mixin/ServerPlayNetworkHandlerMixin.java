package net.epiphany.srvvlcmmndblcks.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import net.minecraft.server.network.ServerPlayNetworkHandler;
import net.minecraft.server.network.ServerPlayerEntity;

/*
 * MIT License
 *
 * Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

@Mixin(ServerPlayNetworkHandler.class)
public class ServerPlayNetworkHandlerMixin {
    /**
     * Allows players to set the command inside a command block in survival mode.
     */
    @Redirect( method = "Lnet/minecraft/server/network/ServerPlayNetworkHandler;onUpdateCommandBlock(Lnet/minecraft/network/packet/c2s/play/UpdateCommandBlockC2SPacket;)V"
             , at = @At( value = "INVOKE"
                       , target = "Lnet/minecraft/server/network/ServerPlayerEntity;isCreativeLevelTwoOp()Z")
             , require = 1)
    private boolean allowUpdateCommandBlock(ServerPlayerEntity self) {
        return true;
    }

    /**
     * Allows players to set the command inside a command block minecart in survival mode.
     */
    @Redirect( method = "Lnet/minecraft/server/network/ServerPlayNetworkHandler;onUpdateCommandBlockMinecart(Lnet/minecraft/network/packet/c2s/play/UpdateCommandBlockMinecartC2SPacket;)V"
             , at = @At( value = "INVOKE"
                       , target = "Lnet/minecraft/server/network/ServerPlayerEntity;isCreativeLevelTwoOp()Z")
             , require = 1)
    private boolean allowUpdateCommandBlockMinecart(ServerPlayerEntity self) {
        return true;
    }
}
