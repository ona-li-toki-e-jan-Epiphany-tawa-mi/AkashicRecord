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
# Runs on reactive plating item entities that are on an anvil, ready to upgrade
#   nearby items.
#
# Parameters:
#   @s - the reactive plating item entity.
#   Location - at @s.
#

# Attempts to make nearby items reactive.
scoreboard players set _made_reactive xplsvtlts 0
execute as @e[type=item,distance=..1] run function xplsvtlts:reactive_plating/crafting/_try_make_reactive

# Immersion baby.
execute if score _made_reactive xplsvtlts matches 1 run particle minecraft:end_rod ~ ~ ~ 0.1 0.1 0.1 0.5 25
execute if score _made_reactive xplsvtlts matches 1 positioned ~ ~-0.25 ~ run function xplsvtlts:anvil/simulate_useage

# Each plating can only upgrade 1 item.
execute if score _made_reactive xplsvtlts matches 1 run kill @s
scoreboard players reset _made_reactive xplsvtlts
