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
# Called when a player blocks damage using a reactive shield.
#
# Parameters:
#   @s - the player.
#   Location - at @s.
#

advancement revoke @s only xplsvtlts:event_listeners/on_hurt/reactive_shield_block



# Can consume 1 gunpowder if shifting for a larger explosion.
scoreboard players set _use_larger_explosion xplsvtlts 0
execute if predicate xplsvtlts:entity/is_shifting store success score _use_larger_explosion xplsvtlts run clear @s minecraft:gunpowder 1

execute if score _use_larger_explosion xplsvtlts matches 0 run summon creeper ^ ^1 ^0.25 {Fuse:0s,ExplosionRadius:1b,"CustomName":'{"text":"Reactive Shield Explosion"}',"CustomNameVisible":false}
execute if score _use_larger_explosion xplsvtlts matches 1 run summon creeper ^ ^1 ^0.25 {Fuse:0s,ExplosionRadius:2b,"CustomName":'{"text":"Reactive Shield Explosion"}',"CustomNameVisible":false}

scoreboard players reset _use_larger_explosion xplsvtlts

