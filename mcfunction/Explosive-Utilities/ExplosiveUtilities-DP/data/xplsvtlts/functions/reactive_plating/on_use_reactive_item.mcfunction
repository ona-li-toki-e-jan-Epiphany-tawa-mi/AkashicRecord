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
# Called when an entity uses a reactive item, i.e. attacking, right-clicking
#   blocks or entities, etc..
#
# Parameters:
#   @s - the entity.
#   Location - at @s.
#

advancement revoke @s only xplsvtlts:event_listeners/on_hit/hit_with_reactive_item
advancement revoke @s only xplsvtlts:event_listeners/on_use/strip_block_with_reactive_axe
advancement revoke @s only xplsvtlts:event_listeners/on_use/use_reactive_shovel_on_block
advancement revoke @s only xplsvtlts:event_listeners/on_use/till_with_reactive_hoe
advancement revoke @s only xplsvtlts:event_listeners/on_use/use_reactive_shears_on_block
advancement revoke @s only xplsvtlts:event_listeners/on_use/use_reactive_shears_on_entity
advancement revoke @s only xplsvtlts:event_listeners/on_use/use_error_error_on_error



# High resistance to protect against the item's explosion.
effect give @s minecraft:resistance 1 4 true

# Raycasts out to the nearest block or entity to try and create the explosion
#   with the thing the player interacted with.
scoreboard players set _max_distance xplsvtlts 6
scoreboard players set _collide_with_entities xplsvtlts 1
function xplsvtlts:raycast/raycast_from_entity
execute at @e[type=minecraft:armor_stand,tag=xplsvtlts_raycast_marker,limit=1,sort=nearest] run summon creeper ~ ~ ~ {Fuse:0s,ExplosionRadius:1b,"CustomName":'{"text":"Reactive Item Explosion"}',"CustomNameVisible":false}
function xplsvtlts:raycast/cleanup
scoreboard players reset _max_distance xplsvtlts
scoreboard players reset _collide_with_entities xplsvtlts

execute if entity @s[gamemode=!creative] run item modify entity @s weapon xplsvtlts:reactive_plating/wear_and_tear/reactive_wear_no_unbreaking
execute if entity @s[gamemode=!creative] run item modify entity @s weapon xplsvtlts:reactive_plating/wear_and_tear/reactive_wear_unbreaking_i
execute if entity @s[gamemode=!creative] run item modify entity @s weapon xplsvtlts:reactive_plating/wear_and_tear/reactive_wear_unbreaking_ii
execute if entity @s[gamemode=!creative] run item modify entity @s weapon xplsvtlts:reactive_plating/wear_and_tear/reactive_wear_unbreaking_iii
