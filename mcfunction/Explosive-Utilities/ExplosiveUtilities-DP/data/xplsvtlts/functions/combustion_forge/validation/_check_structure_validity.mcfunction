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
# Tests if a combustion forge multiblock is valid and complete.
#
# Parameters: 
#   @s - the combustion forge core.
#   Location - at @s.
#
# Returns:
#   execute result - 1 if valid, else 0.
#

execute positioned ^1 ^-0.75 ^1 unless block ~ ~ ~ minecraft:obsidian run return 0
execute positioned ^ ^-0.75 ^1 unless block ~ ~ ~ minecraft:obsidian run return 0
execute positioned ^-1 ^-0.75 ^1 unless block ~ ~ ~ minecraft:obsidian run return 0

execute positioned ^1 ^-0.75 ^ unless block ~ ~ ~ minecraft:obsidian run return 0
execute positioned ^ ^-0.75 ^ unless block ~ ~ ~ minecraft:obsidian run return 0
execute positioned ^-1 ^-0.75 ^ unless block ~ ~ ~ minecraft:obsidian run return 0

execute positioned ^1 ^-0.75 ^-1 unless block ~ ~ ~ minecraft:target run return 0
execute positioned ^ ^-0.75 ^-1 unless block ~ ~ ~ minecraft:obsidian run return 0
execute positioned ^-1 ^-0.75 ^-1 unless block ~ ~ ~ minecraft:target run return 0



execute positioned ^1 ^0.25 ^1 unless block ~ ~ ~ minecraft:obsidian run return 0
execute if entity @s[y_rotation=-95.0..-85.0] positioned ^ ^0.25 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=west,half=top] run return 0
execute if entity @s[y_rotation=85.0..95.0] positioned ^ ^0.25 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=east,half=top] run return 0
execute if entity @s[y_rotation=-5.0..5.0] positioned ^ ^0.25 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=north,half=top] run return 0
execute if entity @s[y_rotation=175.0..185.0] positioned ^ ^0.25 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=south,half=top] run return 0
execute positioned ^-1 ^0.25 ^1 unless block ~ ~ ~ minecraft:obsidian run return 0

execute positioned ^1 ^0.25 ^ unless block ~ ~ ~ minecraft:obsidian run return 0
execute positioned ^ ^0.25 ^ unless block ~ ~ ~ #minecraft:replaceable run return 0
execute positioned ^-1 ^0.25 ^ unless block ~ ~ ~ minecraft:obsidian run return 0

execute positioned ^1 ^0.25 ^-1 unless block ~ ~ ~ minecraft:obsidian run return 0
execute if entity @s[y_rotation=-95.0..-85.0] positioned ^ ^0.25 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=east] run return 0
execute if entity @s[y_rotation=85.0..95.0] positioned ^ ^0.25 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=west] run return 0
execute if entity @s[y_rotation=-5.0..5.0] positioned ^ ^0.25 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=south] run return 0
execute if entity @s[y_rotation=175.0..185.0] positioned ^ ^0.25 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=north] run return 0
execute positioned ^-1 ^0.25 ^-1 unless block ~ ~ ~ minecraft:obsidian run return 0



execute positioned ^1 ^1.25 ^1 unless block ~ ~ ~ minecraft:furnace run return 0
execute positioned ^ ^1.25 ^1 unless block ~ ~ ~ minecraft:furnace run return 0
execute positioned ^-1 ^1.25 ^1 unless block ~ ~ ~ minecraft:furnace run return 0

execute positioned ^1 ^1.25 ^ unless block ~ ~ ~ minecraft:furnace run return 0
execute positioned ^ ^1.25 ^ unless block ~ ~ ~ minecraft:furnace run return 0
execute positioned ^-1 ^1.25 ^ unless block ~ ~ ~ minecraft:furnace run return 0

execute positioned ^1 ^1.25 ^-1 unless block ~ ~ ~ minecraft:furnace run return 0
execute positioned ^ ^1.25 ^-1 unless block ~ ~ ~ minecraft:furnace run return 0
execute positioned ^-1 ^1.25 ^-1 unless block ~ ~ ~ minecraft:furnace run return 0



return 1
