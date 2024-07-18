################################################################################
# MIT License                                                                  #
#                                                                              #
# Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi                        #
#                                                                              #
# Permission is hereby granted, free of charge, to any person obtaining a copy #
# of this software and associated documentation files (the "Software"), to     #
# deal in the Software without restriction, including without limitation the   #
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  #
# sell copies of the Software, and to permit persons to whom the Software is   #
# furnished to do so, subject to the following conditions:                     #
#                                                                              #
# The above copyright notice and this permission notice shall be included in   #
# all copies or substantial portions of the Software.                          #
#                                                                              #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS #
# IN THE SOFTWARE.                                                             #
################################################################################
# Dumps any items in the pistol kiln's input furnaces out as item entities
#   inside it.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# This system relies on using a modified version of the yellow shulker box's
#   loot table that causes it to drop its contents, but not itself, if mined
#   with minecraft:air{drop_contents:1b}. This nbt data cannot occur naturally,
#   but can be supplied in the loot command.
setblock ~ ~ ~ minecraft:yellow_shulker_box



# We copy the items from the furnace into the shulker box, clear it, use the
#   loot table to dump out the items, clear the shulker box, rinse and repeat
#   for the entire grid.
data modify block ~ ~ ~ Items append from block ~1 ~ ~ Items[]
data modify block ~1 ~ ~ Items set value []
loot spawn ~ ~ ~ mine ~ ~ ~ minecraft:air{drop_contents:1b}
data modify block ~ ~ ~ Items set value []

data modify block ~ ~ ~ Items append from block ~-1 ~ ~ Items[]
data modify block ~-1 ~ ~ Items set value []
loot spawn ~ ~ ~ mine ~ ~ ~ minecraft:air{drop_contents:1b}
data modify block ~ ~ ~ Items set value []

data modify block ~ ~ ~ Items append from block ~ ~ ~1 Items[]
data modify block ~ ~ ~1 Items set value []
loot spawn ~ ~ ~ mine ~ ~ ~ minecraft:air{drop_contents:1b}
data modify block ~ ~ ~ Items set value []

data modify block ~ ~ ~ Items append from block ~ ~ ~-1 Items[]
data modify block ~ ~ ~-1 Items set value []
loot spawn ~ ~ ~ mine ~ ~ ~ minecraft:air{drop_contents:1b}
data modify block ~ ~ ~ Items set value []



setblock ~ ~ ~ minecraft:air
