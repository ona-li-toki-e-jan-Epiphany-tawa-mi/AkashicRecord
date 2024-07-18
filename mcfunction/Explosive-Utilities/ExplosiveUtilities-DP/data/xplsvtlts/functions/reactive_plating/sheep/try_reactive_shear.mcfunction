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
# The advancement player_interacted_with_entity shows the entity after it has
#   been interacted with.
# Because of this, we cannot directly differentiate between a sheep sheared
#   through the interaction, or a sheep that was already sheared beforehand.
# This solution relies on placing a tag on sheep that were already sheared to
#   isolate them, so that this only reacts to sheep that were sheared through
#   the interaction that called this function.
#
# Parameters:
#   @s - the player.
#   Location - at @s.
#

advancement revoke @s only xplsvtlts:event_listeners/on_use/use_reactive_shears_on_sheep



execute if entity @e[type=minecraft:sheep,tag=!xplsvtlts_already_sheared,sort=nearest,limit=1,nbt={"Sheared":1b},distance=..6] run function xplsvtlts:reactive_plating/on_use_reactive_item
tag @e[type=minecraft:sheep,tag=!xplsvtlts_already_sheared,sort=nearest,limit=1,nbt={"Sheared":1b},distance=..6] add xplsvtlts_already_sheared
