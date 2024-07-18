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
# Runs once every second on valid, but inactive, combustion forges.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#

# The forge can only try to start up once the door is closed.
execute unless block ^ ^ ^1 minecraft:iron_trapdoor[powered=true] run return 0



# Tries to consume tnt as fuel to start up.
function xplsvtlts:combustion_forge/ideling/_try_consume_fuel

# Marks that the forge is now running.
execute if score _consumed_fuel xplsvtlts matches 1 run tag @s add xplsvtlts_is_running
# Redstone I/O to indicate that the forge is running for automation.
execute if score _consumed_fuel xplsvtlts matches 1 positioned ^-1 ^-1 ^-1 if block ~ ~ ~ minecraft:target run setblock ~ ~ ~ minecraft:target[power=15]

# Kickstarts ticking for active forges.
execute if score _consumed_fuel xplsvtlts matches 1 run schedule function xplsvtlts:combustion_forge/processing/tick_active_cores 1t



scoreboard players reset _consumed_fuel xplsvtlts
