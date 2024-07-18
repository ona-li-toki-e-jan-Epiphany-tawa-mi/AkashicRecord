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
# Attempts to craft up to 16 items using the ingridients specified in the
#   combustion forge's crafting grid, consuming those ingredients in the process
#   and outputting the results as item entities inside the combustion forge.
#
# Parameters:
#   @s - the combustion forge.
#   Location - at @s.
#

# Prevents running recipe functions if crafting grid is empty.
scoreboard players set _empty_slot_count xplsvtlts 0
execute unless block ^-1 ^1 ^-1 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^0 ^1 ^-1 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^1 ^1 ^-1 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^-1 ^1 ^0 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^0 ^1 ^0 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^1 ^1 ^0 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^-1 ^1 ^1 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^0 ^1 ^1 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1
execute unless block ^1 ^1 ^1 minecraft:furnace{Items:[{Slot:0b}]} run scoreboard players add _empty_slot_count xplsvtlts 1



scoreboard players set _items_crafted xplsvtlts 0
scoreboard players set _found_recipe xplsvtlts 0

# See {PROJECT_DIR}/combustion_forge_recipes/ for adding more recipes to the
#   combustion forge.
execute if score _empty_slot_count xplsvtlts matches ..8 run function #xplsvtlts:combustion_forge_recipes

scoreboard players reset _items_crafted xplsvtlts
scoreboard players reset _found_recipe xplsvtlts
scoreboard players reset _valid_ingredient_count xplsvtlts



scoreboard players reset _empty_slot_count xplsvtlts
