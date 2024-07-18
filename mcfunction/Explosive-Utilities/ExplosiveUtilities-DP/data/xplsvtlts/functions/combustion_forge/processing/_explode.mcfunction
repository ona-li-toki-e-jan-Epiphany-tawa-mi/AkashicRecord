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
# Creates the explosion that forms the final operation of the combustion forge
#   to damage nearby entities and eject the outputted items.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#

# We need to create a fake explosion here because we don't want to destory the
#   items inside the forge.

# A/v effects to create a faux explosion.
playsound minecraft:entity.generic.explode block @a ~ ~ ~ 2.0 1.0
playsound minecraft:entity.generic.explode block @a ~ ~ ~ 2.0 1.0
particle minecraft:explosion_emitter ~ ~ ~ 0 0 0 0.0 1

# Damages entites, more for those in front of the forge.
execute positioned ^ ^ ^3 as @e[distance=..3,predicate=!xplsvtlts:entity/is_ignorable] run damage @s 7.5 minecraft:explosion 
execute as @e[distance=..6,predicate=!xplsvtlts:entity/is_ignorable] run damage @s 1.0 minecraft:explosion 

# Blasts items out.
execute if entity @s[y_rotation=-95.0..-85.0] as @e[type=minecraft:item,distance=..1] run data modify entity @s Motion set value [1.0,0.2,0.0]
execute if entity @s[y_rotation=85.0..95.0] as @e[type=minecraft:item,distance=..1] run data modify entity @s Motion set value [-1.0,0.2,0.0]
execute if entity @s[y_rotation=-5.0..5.0] as @e[type=minecraft:item,distance=..1] run data modify entity @s Motion set value [0.0,0.2,1.0]
execute if entity @s[y_rotation=175.0..185.0] as @e[type=minecraft:item,distance=..1] run data modify entity @s Motion set value [0.0,0.2,-1.0]
