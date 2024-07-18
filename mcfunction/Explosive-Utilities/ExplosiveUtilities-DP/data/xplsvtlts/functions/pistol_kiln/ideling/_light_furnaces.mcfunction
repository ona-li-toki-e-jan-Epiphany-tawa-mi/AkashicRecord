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
# Lights the furnaces of the pistol kiln for a cool visual effect.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# Temporary inventory.
setblock ~ ~ ~ minecraft:yellow_shulker_box



# The items have to to be copied from the furnaces to the shulker and back 
#   because using setblock to light the furnaces will clear their inventories.
execute if block ~1 ~ ~ minecraft:blast_furnace run data modify block ~ ~ ~ Items append from block ~1 ~ ~ Items[]
execute positioned ~1 ~ ~ if block ~ ~ ~ minecraft:blast_furnace run setblock ~ ~ ~ minecraft:blast_furnace[lit=true,facing=east]
execute if block ~1 ~ ~ minecraft:blast_furnace run data modify block ~1 ~ ~ Items append from block ~ ~ ~ Items[]
execute if block ~1 ~ ~ minecraft:blast_furnace run data modify block ~ ~ ~ Items set value []

execute if block ~-1 ~ ~ minecraft:blast_furnace run data modify block ~ ~ ~ Items append from block ~-1 ~ ~ Items[]
execute positioned ~-1 ~ ~ if block ~ ~ ~ minecraft:blast_furnace run setblock ~ ~ ~ minecraft:blast_furnace[lit=true,facing=west]
execute if block ~-1 ~ ~ minecraft:blast_furnace run data modify block ~-1 ~ ~ Items append from block ~ ~ ~ Items[]
execute if block ~-1 ~ ~ minecraft:blast_furnace run data modify block ~ ~ ~ Items set value []

execute if block ~ ~ ~1 minecraft:blast_furnace run data modify block ~ ~ ~ Items append from block ~ ~ ~1 Items[]
execute positioned ~ ~ ~1 if block ~ ~ ~ minecraft:blast_furnace run setblock ~ ~ ~ minecraft:blast_furnace[lit=true,facing=south]
execute if block ~ ~ ~1 minecraft:blast_furnace run data modify block ~ ~ ~1 Items append from block ~ ~ ~ Items[]
execute if block ~ ~ ~1 minecraft:blast_furnace run data modify block ~ ~ ~ Items set value []

execute if block ~ ~ ~-1 minecraft:blast_furnace run data modify block ~ ~ ~ Items append from block ~ ~ ~-1 Items[]
execute positioned ~ ~ ~-1 if block ~ ~ ~ minecraft:blast_furnace run setblock ~ ~ ~ minecraft:blast_furnace[lit=true,facing=north]
execute if block ~ ~ ~-1 minecraft:blast_furnace run data modify block ~ ~ ~-1 Items append from block ~ ~ ~ Items[]



setblock ~ ~ ~ minecraft:air
