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
# Called when a player uses dynamite.
#
# Parameters:
#   @s - the player.
#

advancement revoke @s only xplsvtlts:event_listeners/on_use/use_dynamite
execute if entity @s[gamemode=!creative] run clear @s minecraft:armor_stand{CustomModelData:15704532} 1



# Creates thrown dynamite entity.
function xplsvtlts:vector3d/get_rotation_vector
scoreboard players set _scalar xplsvtlts 3
function xplsvtlts:vector3d/scalar_multiply
scoreboard players set _scalar xplsvtlts 2
function xplsvtlts:vector3d/scalar_divide
execute anchored eyes positioned ^ ^ ^ summon minecraft:armor_stand run function xplsvtlts:dynamite/_make_dynamite



scoreboard players reset _scalar xplsvtlts
function xplsvtlts:vector3d/reset_memory
