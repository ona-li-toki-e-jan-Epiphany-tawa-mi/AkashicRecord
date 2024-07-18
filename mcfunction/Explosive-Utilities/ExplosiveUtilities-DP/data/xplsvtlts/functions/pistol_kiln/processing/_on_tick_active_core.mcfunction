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
# Runs once every tick for active pistol kilns.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# Determines whether the kiln should progress or reverse based on if it has
#   enough fuel and something to smelt.
scoreboard players set _progress_direction xplsvtlts 1
execute unless block ~1 ~ ~ minecraft:blast_furnace{Items:[{Slot:0b}]} unless block ~-1 ~ ~ minecraft:blast_furnace{Items:[{Slot:0b}]} unless block ~ ~ ~1 minecraft:blast_furnace{Items:[{Slot:0b}]} unless block ~ ~ ~-1 minecraft:blast_furnace{Items:[{Slot:0b}]} run scoreboard players set _progress_direction xplsvtlts -1

# Consumes fuel over time.
execute unless score _progress_direction xplsvtlts matches -1 if score @s xplsvtlts_pistol_kiln_fuel_time matches 1.. run scoreboard players remove @s xplsvtlts_pistol_kiln_fuel_time 1
execute unless score _progress_direction xplsvtlts matches -1 run execute if score @s xplsvtlts_pistol_kiln_fuel_time matches ..0 run function xplsvtlts:pistol_kiln/try_consume_fuel
execute unless score _progress_direction xplsvtlts matches -1 run execute if score @s xplsvtlts_pistol_kiln_fuel_time matches ..0 if score _consumed_fuel xplsvtlts matches 1 run scoreboard players operation @s xplsvtlts_pistol_kiln_fuel_time += pistol_kiln_gunpowder_fuel_time xplsvtlts
execute unless score _progress_direction xplsvtlts matches -1 run execute if score @s xplsvtlts_pistol_kiln_fuel_time matches ..0 run scoreboard players reset _consumed_fuel xplsvtlts
execute unless score _progress_direction xplsvtlts matches -1 run execute if score @s xplsvtlts_pistol_kiln_fuel_time matches ..0 run scoreboard players set _progress_direction xplsvtlts -1

scoreboard players operation @s xplsvtlts_pistol_kiln_runtime += _progress_direction xplsvtlts



function xplsvtlts:pistol_kiln/processing/_heat_inside

# Hella smog baby!
execute if score _progress_direction xplsvtlts matches 1 run particle minecraft:campfire_signal_smoke ~ ~ ~ 0 0 0 0.1 1

execute if score @s xplsvtlts_pistol_kiln_runtime matches ..0 run function xplsvtlts:pistol_kiln/processing/_mark_inactive
execute if score @s xplsvtlts_pistol_kiln_runtime matches ..0 run function xplsvtlts:pistol_kiln/processing/_unlight_furnaces
# Won't run if there is a block in the core to prevent overwriting it, the
#   structural validator will catch the issue later on.
execute if score @s xplsvtlts_pistol_kiln_runtime > pistol_kiln_runtime xplsvtlts if block ~ ~ ~ #minecraft:replaceable run function xplsvtlts:pistol_kiln/processing/_deactivate



scoreboard players reset _progress_direction xplsvtlts
