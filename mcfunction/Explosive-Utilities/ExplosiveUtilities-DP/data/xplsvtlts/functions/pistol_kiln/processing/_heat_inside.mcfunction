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
# Used to fill the inside of the kiln with fire and deal extra damage to those
#   inside.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# Mod 7 for every 7 ticks.
scoreboard players operation _runtime_mod_7 xplsvtlts = @s xplsvtlts_combustion_forge_runtime
scoreboard players operation _runtime_mod_7 xplsvtlts %= #7 xplsvtlts

execute if score _runtime_mod_7 xplsvtlts matches 0 as @e[predicate=!xplsvtlts:entity/is_ignorable,dx=0.5,dy=1.0,dz=0.5] run damage @s 2.0 minecraft:in_fire
execute if score _runtime_mod_7 xplsvtlts matches 0 run setblock ~ ~ ~ minecraft:fire

scoreboard players reset _runtime_mod_7 xplsvtlts
