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
# Deactivates a running combustion forge, marking it inactive, and updating
#   redstone I/O, starting recipe checking, and ejecting results and left over
#   items.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#

# Updates redstone I/0 to indicate it is no longer running.
execute positioned ^-1 ^-1 ^-1 if block ~ ~ ~ minecraft:target run setblock ~ ~ ~ minecraft:target[power=0]
# Updates redstone I/0 to indicate it is no longer finished processing if it was.
execute positioned ^1 ^-1 ^-1 if block ~ ~ ~ minecraft:target run setblock ~ ~ ~ minecraft:target[power=0]



# If finished, attempts to craft items with what is in the crafting grid.
execute if score @s xplsvtlts_combustion_forge_runtime matches 120.. run function xplsvtlts:combustion_forge/crafting/try_craft
# Dumps any leftover items in the crafting grid out inside the combustion forge.
function xplsvtlts:combustion_forge/processing/_move_leftovers_to_output
# Blasts any outputted items out of the forge.
function xplsvtlts:combustion_forge/processing/_explode



# Marks inactive.
tag @s remove xplsvtlts_is_running
# Kickstarts tick loop for inactive forges.
schedule function xplsvtlts:combustion_forge/ideling/tick_inactive_cores 1s
# Resets runtime for next time the forge runs.
scoreboard players reset @s xplsvtlts_combustion_forge_runtime
