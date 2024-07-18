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
# Attempts to corral entities compatible with the tnt wand around the given
#   location.
#
# Parameters:
#   @s - the player using the tnt wand.
#   Location - where to corral.
#
# Returns:
#   _corraled_tnt (scoreboard: xplsvtlts) - 1 if any entities were corraled, 0
#       otherwise.
#

scoreboard players set _corraled_tnt xplsvtlts 0
function xplsvtlts:vector3d/get_location_as_vector
function xplsvtlts:vector3d/store/3

execute unless predicate xplsvtlts:entity/tnt_wand/whatawawtawtaw store success score _corraled_tnt xplsvtlts run execute as @e[distance=..5,predicate=xplsvtlts:entity/tnt_wand/can_be_corraled] run function xplsvtlts:tnt_wand/corraling/_corral_entity
execute if predicate xplsvtlts:entity/tnt_wand/whatawawtawtaw store success score _corraled_tnt xplsvtlts run execute as @e[distance=..5,type=!minecraft:player,predicate=!xplsvtlts:entity/is_ignorable_no_items] run function xplsvtlts:tnt_wand/corraling/_corral_entity
