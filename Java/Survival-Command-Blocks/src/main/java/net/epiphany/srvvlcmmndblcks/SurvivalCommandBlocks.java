package net.epiphany.srvvlcmmndblcks;

import org.jetbrains.annotations.Nullable;

import net.epiphany.srvvlcmmndblcks.mixin.AbstractBlockAccessor;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.mixin.object.builder.AbstractBlockSettingsAccessor;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Blocks;
import net.minecraft.util.Identifier;

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

/**
 * A mod that allows players in survival mode to craft, place, use, and break command blocks.
 */
public class SurvivalCommandBlocks implements ModInitializer {
	public static final String MINECRAFT_NAMESPACE = "minecraft";

	@Override
	public void onInitialize() {
		// By default, the command blocks do not have a loot table defined.
		changeLootTableID(Blocks.COMMAND_BLOCK, new Identifier(MINECRAFT_NAMESPACE, "blocks/command_block"));
		changeLootTableID(Blocks.CHAIN_COMMAND_BLOCK, new Identifier(MINECRAFT_NAMESPACE, "blocks/chain_command_block"));
		changeLootTableID(Blocks.REPEATING_COMMAND_BLOCK, new Identifier(MINECRAFT_NAMESPACE, "blocks/repeating_command_block"));
	}



	/**
	 * Changes the loot table of a preexisting block.
	 *
	 * @param block       The block to change the loot table of.
	 * @param lootTableID The loot table to use.
	 */
	public static void changeLootTableID(AbstractBlock block, @Nullable Identifier lootTableID) {
		AbstractBlockAccessor blockAccessor = (AbstractBlockAccessor) block;

		blockAccessor.setLootTableID(lootTableID);
		((AbstractBlockSettingsAccessor) blockAccessor.getSettings()).setLootTableId(lootTableID);
	}
}
