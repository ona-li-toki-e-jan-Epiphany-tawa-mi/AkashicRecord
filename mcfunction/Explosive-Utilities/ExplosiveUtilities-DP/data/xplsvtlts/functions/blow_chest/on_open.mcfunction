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
# Called when a player opens a blow chest.
#
# Parameters:
#   @s - the player.
#

advancement revoke @s only xplsvtlts:event_listeners/on_use/open_blow_chest



# Raycasts out to try and find the blow chest the player is looking at. If not 
#   found, we just leave whatever there be as nothing else can be done.
scoreboard players set _max_distance xplsvtlts 6
scoreboard players set _collide_with_entities xplsvtlts 0
function xplsvtlts:raycast/raycast_from_entity
execute at @e[type=minecraft:armor_stand,tag=xplsvtlts_raycast_marker,limit=1,sort=nearest] if predicate xplsvtlts:block/is_blow_chest run function xplsvtlts:blow_chest/_explode_chest
function xplsvtlts:raycast/cleanup
scoreboard players reset _max_distance xplsvtlts
scoreboard players reset _collide_with_entities xplsvtlts
