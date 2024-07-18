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
# Has the player do an explosive punch that causes massive knockback.
#
# Parameters:
#   @s - the player.
#   Location - at @s.
#

# High resistance to prevent immediate death.
effect give @s minecraft:resistance 1 3 true

execute anchored eyes positioned ^ ^-0.25 ^1 summon minecraft:creeper run function xplsvtlts:tnt_wand/punch/_create_explosion

# Sets cooldown to whatever is in the config.
scoreboard players operation @s xplsvtlts_tnt_wand_punch_cooldown = tnt_wand_punch_cooldown xplsvtlts
# Kickstarts cooldown ticking.
schedule function xplsvtlts:tnt_wand/cooldown/tick_punch_cooldowns 1t
