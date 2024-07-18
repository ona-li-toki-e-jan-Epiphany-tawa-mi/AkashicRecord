package net.epiphany.srvvlcmmndblcks.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import com.mojang.brigadier.tree.CommandNode;

import net.minecraft.server.command.CommandManager;

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

@Mixin(CommandManager.class)
public class CommandManagerMixin {
    /**
     * Allows players to see all server commands and suggestions for them even if they are not an operator.
     * Note: this doesn't allow them to execute the commands, just to look at them.
     */
    @SuppressWarnings({"rawtypes", "mapping"}) /* Says cannot find mapping and warnings appear in console,
                                                *   but everything works hunky-dory so idk.
                                                */
    @Redirect( method = "Lnet/minecraft/server/command/CommandManager;makeTreeForSource(Lcom/mojang/brigadier/tree/CommandNode;Lcom/mojang/brigadier/tree/CommandNode;Lnet/minecraft/server/command/ServerCommandSource;Ljava/util/Map;)V"
             , at = @At( value = "INVOKE"
                       , target = "Lcom/mojang/brigadier/tree/CommandNode;canUse(Ljava/lang/Object;)Z")
             , require = 1)
    private boolean allowSeeCommandSuggestions(CommandNode self, Object source) {
        return true;
    }
}
