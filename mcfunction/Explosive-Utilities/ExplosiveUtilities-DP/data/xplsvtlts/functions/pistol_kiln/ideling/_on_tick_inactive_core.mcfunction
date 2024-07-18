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
# Runs once every second on valid, but inactive, pistol kilns.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# The kiln can only try to start up once there are items in the input furnaces.
execute unless block ~1 ~ ~ minecraft:blast_furnace{Items:[{Slot:0b}]} unless block ~-1 ~ ~ minecraft:blast_furnace{Items:[{Slot:0b}]} unless block ~ ~ ~1 minecraft:blast_furnace{Items:[{Slot:0b}]} unless block ~ ~ ~-1 minecraft:blast_furnace{Items:[{Slot:0b}]} run return 0
# Won't run if there is a block in the core to prevent overwriting it, the 
#   structural validator will catch the issue later on.
execute unless block ~ ~ ~ #minecraft:replaceable run return 0



# Tries to consume gunpowder as fuel to start up.
function xplsvtlts:pistol_kiln/try_consume_fuel
# Adds consumed fuel to fuel timer
execute if score _consumed_fuel xplsvtlts matches 1 run scoreboard players operation @s xplsvtlts_pistol_kiln_fuel_time += pistol_kiln_gunpowder_fuel_time xplsvtlts

# Marks that the kiln is now running.
execute if score _consumed_fuel xplsvtlts matches 1 run tag @s add xplsvtlts_is_running
# A E S T E T I C or smth idk.
execute if score _consumed_fuel xplsvtlts matches 1 run function xplsvtlts:pistol_kiln/ideling/_light_furnaces
# Kickstarts ticking for active kilns.
execute if score _consumed_fuel xplsvtlts matches 1 run schedule function xplsvtlts:pistol_kiln/processing/tick_active_cores 1t



scoreboard players reset _consumed_fuel xplsvtlts
