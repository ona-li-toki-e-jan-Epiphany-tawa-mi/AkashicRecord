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
# Called when a player crafts dynamite. Small chance to explode ;).
#
# Parameters:
#   @s - the player.
#   Location - at @s.
#

recipe take @s xplsvtlts:stonecutting/dynamite
advancement revoke @s only xplsvtlts:event_listeners/on_craft/craft_dynamite
clear @s minecraft:knowledge_book 



scoreboard players set _exploded xplsvtlts 0
execute store success score _exploded xplsvtlts run execute if predicate xplsvtlts:entity/dynamite/should_crafting_explode run summon minecraft:tnt ~ ~ ~ {Fuse:0s,CustomName:'{"text":"Dynamite Crafting Explosion"}',CustomNameVisible:0b}
execute if score _exploded xplsvtlts matches 0 run give @p minecraft:armor_stand{display:{Name:'{"text":"Dynamite","italic":false}',Lore:['{"text":"Explosive Utilities","color":"blue","italic":false}']},CustomModelData:15704532,EntityTag:{Invulnerable:1b,Small:1b,Invisible:1b,Tags:["xplsvtlts_dynamite"],ArmorItems:[{},{},{},{id:"minecraft:armor_stand",Count:1b,tag:{CustomModelData:15704532}}]}} 16



scoreboard players reset _exploded xplsvtlts
