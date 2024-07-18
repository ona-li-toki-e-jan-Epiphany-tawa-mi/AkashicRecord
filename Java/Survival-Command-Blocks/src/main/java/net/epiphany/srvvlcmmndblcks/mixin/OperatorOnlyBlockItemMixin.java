package net.epiphany.srvvlcmmndblcks.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.CommandBlock;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.BlockItem;
import net.minecraft.item.ItemPlacementContext;
import net.minecraft.item.OperatorOnlyBlockItem;

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

@Mixin(OperatorOnlyBlockItem.class)
public class OperatorOnlyBlockItemMixin {
    /**
     * Allows operator block items to be placed even if the user is in survival mode.
     */
    @Redirect( method = "Lnet/minecraft/item/OperatorOnlyBlockItem;getPlacementState(Lnet/minecraft/item/ItemPlacementContext;)Lnet/minecraft/block/BlockState;"
             , at = @At( value = "INVOKE"
                       , target = "Lnet/minecraft/entity/player/PlayerEntity;isCreativeLevelTwoOp()Z")
             , require = 1)
    private boolean allowPlaceOperatorBlock(PlayerEntity self) {
        return true;
    }

    /**
     * Restricts the placing of operator blocks to command blocks for players in survival mode.
     */
    @Inject( method = "Lnet/minecraft/item/OperatorOnlyBlockItem;getPlacementState(Lnet/minecraft/item/ItemPlacementContext;)Lnet/minecraft/block/BlockState;"
           , at = @At("HEAD")
           , cancellable = true
           , require = 1)
    private void allowPlaceOnlyCommandBlock(ItemPlacementContext context, CallbackInfoReturnable<BlockState> info) {
        // If the player is a valid operator anyways there's no need to do anything.
        PlayerEntity player = context.getPlayer();
        
        if (player != null && player.isCreativeLevelTwoOp())
            return;


        Block block = ((BlockItem) context.getStack().getItem()).getBlock();

        if (!(block instanceof CommandBlock))
            info.setReturnValue(null);
    }
}
