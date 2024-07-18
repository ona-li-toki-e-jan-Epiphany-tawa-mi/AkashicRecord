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
# Creates the explosion that forms the final operation of the pistol kiln to
#   damage entities in the way of the kiln and eject the outputted items.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# We need to create a fake explosion here because we don't want to destory the
#   items inside the kiln.

# A/v effects to create a faux explosion.
playsound minecraft:entity.generic.explode block @a ~ ~ ~ 2.0 1.0
playsound minecraft:entity.generic.explode block @a ~ ~ ~ 2.0 1.0
particle minecraft:explosion_emitter ~ ~ ~ 0 0 0 0.0 1

# Damages entities in the way of the kiln
execute positioned ~ ~ ~ as @e[dx=0.5,dy=7,dz=0.5,predicate=!xplsvtlts:entity/is_ignorable] run damage @s 10.0 minecraft:explosion

# Blasts items out.
execute as @e[type=minecraft:item,distance=..1] run data modify entity @s Motion[1] set value 2.0
