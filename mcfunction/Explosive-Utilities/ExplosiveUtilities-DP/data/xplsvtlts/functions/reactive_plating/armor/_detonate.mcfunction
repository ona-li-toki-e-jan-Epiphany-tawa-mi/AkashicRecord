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
# Detonates the reactive armor and applies a cooldown.
#
# Parameters:
#   @s - the bearer of the armor.
#   Location - at @s.
#   _reactive_level (scoreboard: xplsvtlts) - the number of reactive armor items
#       worn by @s.
#

# High resistance to protect against the armor's explosion.
effect give @s minecraft:resistance 1 4 true

# Larger explosion set bonus.
execute if score _reactive_level xplsvtlts matches ..3 run summon creeper ~ ~1 ~ {Fuse:0s,ExplosionRadius:1b,"CustomName":'{"text":"Reactive Armor Explosion"}',"CustomNameVisible":false}
execute if score _reactive_level xplsvtlts matches 4.. run summon creeper ~ ~1 ~ {Fuse:0s,ExplosionRadius:2b,"CustomName":'{"text":"Reactive Armor Explosion"}',"CustomNameVisible":false}

# Set bonus reduces cooldown.
scoreboard players operation @s xplsvtlts_reactive_armor_cooldown = reactive_armor_maximum_cooldown xplsvtlts
scoreboard players operation @s xplsvtlts_reactive_armor_cooldown /= _reactive_level xplsvtlts
# Kickstarts cooldown ticking.
schedule function xplsvtlts:reactive_plating/armor/tick_cooldowns 1t
