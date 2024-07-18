package net.epiphany.srvvlcmmndblcks.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Overwrite;

import net.minecraft.entity.vehicle.CommandBlockMinecartEntity;
import net.minecraft.item.Item;
import net.minecraft.item.Items;

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

@Mixin(CommandBlockMinecartEntity.class)
public class CommandBlockMinecartEntityMixin {
    /**
     * Rewrites the command block minecart to drop itself rather than an empty minecart.
     * 
     * @author ona li toki e jan Epiphany tawa mi.
     * @reason The get item method is extremly simple and any other mod being loaded will likely not modify it either. Even if
     *      they do, the same result would likely be returned.
     */
    @Overwrite
    public Item getItem() {
        return Items.COMMAND_BLOCK_MINECART;
    }
}
