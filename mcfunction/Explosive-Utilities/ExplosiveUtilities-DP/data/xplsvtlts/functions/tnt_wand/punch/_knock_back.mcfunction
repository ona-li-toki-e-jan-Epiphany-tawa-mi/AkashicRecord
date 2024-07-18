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
# Applies the explosive punch knockback to the entity relative to the position
#   stored in vector register 3.
#
# Parameters:
#   @s - the entity.
#   Location - at @s.
#   Vector register 3 - the position of the source of the knockback
#

# Generates desired motion away from marker.
function xplsvtlts:vector3d/load/3
function xplsvtlts:vector3d/swap
function xplsvtlts:vector3d/get_positon
function xplsvtlts:vector3d/subtract
# A little verticality to feel like Minecraft's normal knockback. (~7.5 
#   unscaled.)
scoreboard players set _y1 xplsvtlts 53
function xplsvtlts:vector3d/normalize
scoreboard players set _scalar xplsvtlts 4
function xplsvtlts:vector3d/scalar_multiply

# If the entity is a fireball, we need to set it's acceleration or else nothing 
#   will happen.
execute if data entity @s power run function xplsvtlts:vector3d/set_power

# Adds motion to the entity's current motion.
function xplsvtlts:vector3d/swap
function xplsvtlts:vector3d/get_motion
function xplsvtlts:vector3d/add

function xplsvtlts:vector3d/set_motion



scoreboard players reset _scalar xplsvtlts
