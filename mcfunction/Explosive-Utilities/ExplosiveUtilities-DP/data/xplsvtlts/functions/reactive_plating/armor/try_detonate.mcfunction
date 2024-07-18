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
# Called when an entity is damaged and doesn't have an active reactive armor
#   cooldown.
# Used to check if they have reactive armor and whether it should detonate.
#
# Parameters:
#   @s - the entity to check.
#   Location - at @s.

# Used to scale detonation abilities with how many reactive piecies are worn.
scoreboard players set _reactive_level xplsvtlts 0
execute if predicate xplsvtlts:entity/reactive_plating/is_helmet_reactive run scoreboard players add _reactive_level xplsvtlts 1
execute if predicate xplsvtlts:entity/reactive_plating/is_chestplate_reactive run scoreboard players add _reactive_level xplsvtlts 1
execute if predicate xplsvtlts:entity/reactive_plating/is_leggings_reactive run scoreboard players add _reactive_level xplsvtlts 1
execute if predicate xplsvtlts:entity/reactive_plating/is_boots_reactive run scoreboard players add _reactive_level xplsvtlts 1

execute if score _reactive_level xplsvtlts matches 1.. run function xplsvtlts:reactive_plating/armor/_detonate
scoreboard players reset _reactive_level xplsvtlts
