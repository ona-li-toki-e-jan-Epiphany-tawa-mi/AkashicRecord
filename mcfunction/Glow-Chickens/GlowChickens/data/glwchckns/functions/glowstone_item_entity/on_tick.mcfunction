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
# Ticks glowstone item entities.
#
# Needs to be called with @s being the item entity and located at @s.
#

# Locates the nearest chicken and makes them glow if they are not already
execute store success score _found_chicken glwchckns as @e[type=minecraft:chicken,limit=1,sort=nearest,distance=..1,] unless predicate glwchckns:is_glowing run effect give @s glowing 180 0 false

execute if score _found_chicken glwchckns matches 1 run function glwchckns:glowstone_item_entity/consume
scoreboard players reset _found_chicken glwchckns
