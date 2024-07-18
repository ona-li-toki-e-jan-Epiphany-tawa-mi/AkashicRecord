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
# Corrals tnt in front of the player, allowing them to control it.
#
# Note: this is meant for tnt, but it should work for other things too.
#
# Parameters:
#   @s - the tnt entity.
#   Location - at @s.
#   Vector register 3 - where to corral the tnt to.
#

# Corrals the tnt to the front of the wand.
function xplsvtlts:vector3d/get_positon
function xplsvtlts:vector3d/swap
function xplsvtlts:vector3d/load/3
function xplsvtlts:vector3d/subtract
# Without this, the speed is to quick and the tnt goes flying.
scoreboard players set _scalar xplsvtlts 6
function xplsvtlts:vector3d/scalar_divide
# Since the motion is based on the distance of the tnt to the corral position
#   we get tnt throwing for free.
function xplsvtlts:vector3d/set_motion
# If the entity is a fireball we also need to set the accerleration or else it
#   will try to keep going.
scoreboard players set _scalar xplsvtlts 12
function xplsvtlts:vector3d/scalar_divide
execute if data entity @s power run function xplsvtlts:vector3d/set_power

# Sets the tnt to have no gravity (if that isn't already the case) in case of
#   lag.
execute if data entity @s NoGravity run data modify entity @s NoGravity set value true
# Restores gravity on thawing.
execute if data entity @s NoGravity run tag @s add xplsvtlts_gravity_on_thaw

# Freezes the fuse to make sure explosives don't blow up in the user's face.
scoreboard players set @s xplsvtlts_fuse_freeze_time 20
# Kickstarts frozen fuse handling.
schedule function xplsvtlts:fuse_freezing/tick_frozen_fuses 1t

playsound minecraft:entity.phantom.flap player @a ~ ~ ~ 1.0 0.1
particle minecraft:portal ~ ~ ~ 0.25 0.25 0.25 0.1 10
