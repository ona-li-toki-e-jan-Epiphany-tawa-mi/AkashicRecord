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
# Runs once every tick for active combustion forges.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#

scoreboard players add @s xplsvtlts_combustion_forge_runtime 1

execute if score @s xplsvtlts_combustion_forge_runtime matches 30 run playsound minecraft:block.anvil.land block @a ~ ~ ~ 2.0 0.8
execute if score @s xplsvtlts_combustion_forge_runtime matches 60 run playsound minecraft:block.anvil.land block @a ~ ~ ~ 2.0 0.7
execute if score @s xplsvtlts_combustion_forge_runtime matches 90 run playsound minecraft:block.anvil.land block @a ~ ~ ~ 2.0 0.6
execute if score @s xplsvtlts_combustion_forge_runtime matches ..120 run function xplsvtlts:combustion_forge/processing/_play_processing_sound

# Redstone I/O to indicate the machine is done processing.
execute if score @s xplsvtlts_combustion_forge_runtime matches 120 positioned ^1 ^-1 ^-1 if block ~ ~ ~ minecraft:target run setblock ~ ~ ~ minecraft:target[power=15]

# Once the door opens, the machine stops, finished or not with processing.
# Won't run if there is a block in the core to prevent overwriting it, the
#   structural validator will catch the issue later on.
execute if block ^ ^ ^1 minecraft:iron_trapdoor[powered=false] if block ~ ~ ~ #minecraft:replaceable run function xplsvtlts:combustion_forge/processing/_deactivate
