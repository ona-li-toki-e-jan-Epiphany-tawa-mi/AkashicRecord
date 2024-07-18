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
# Called when a player uses a tnt wand
#
# Parameters:
#   @s - the player.
#   Location - at @s.
#

advancement revoke @s only xplsvtlts:event_listeners/on_use/use_tnt_wand



execute anchored eyes positioned ^ ^-0.25 ^4 run function xplsvtlts:tnt_wand/corraling/try_corral
# Prevents spawning tnt whilst corraling if the user moves to fast or something
#   lags.
execute if score _corraled_tnt xplsvtlts matches 1 run scoreboard players operation @s xplsvtlts_tnt_wand_summon_cooldown = tnt_wand_summon_cooldown xplsvtlts
# Kickstarts cooldown ticking.
execute if score _corraled_tnt xplsvtlts matches 1 run schedule function xplsvtlts:tnt_wand/cooldown/tick_summon_cooldowns 1t


# Tnt summon ability.
execute if score _corraled_tnt xplsvtlts matches 0 unless score @s xplsvtlts_tnt_wand_summon_cooldown matches 1.. anchored eyes positioned ^ ^-0.25 ^4 run function xplsvtlts:tnt_wand/summoning/try_summon_tnt



scoreboard players reset _scalar xplsvtlts
function xplsvtlts:vector3d/reset_memory
scoreboard players reset _corraled_tnt xplsvtlts
