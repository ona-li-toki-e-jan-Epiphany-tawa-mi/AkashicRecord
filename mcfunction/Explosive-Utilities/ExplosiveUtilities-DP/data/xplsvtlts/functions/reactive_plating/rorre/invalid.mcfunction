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
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
# ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN ERROR: UNKNOWN
#

data modify entity @s Item.tag.display.Name set value "{\"text\":\"??dnaW tnT??\",\"obfuscated\":\"true\",\"italic\":false,\"bold\":\"true\",\"color\":\"red\"}"

data modify entity @s Item.tag.display.Lore append value "{\"text\":\"!!!REACTIVE!!!\",\"italic\":false,\"bold\":\"true\",\"color\":\"gold\"}"
data modify entity @s Item.tag.display.Lore append value "{\"text\":\"ma ali suli li moli\",\"bold\":\"true\",\"obfuscated\":\"true\",\"color\":\"purple\"}"
data modify entity @s Item.tag.display.Lore append value "{\"text\":\"You weren't supposed to do that...\",\"bold\":\"true\",\"color\":\"purple\"}"
data modify entity @s Item.tag.display.Lore append value "{\"text\":\"ma ali sin li utala tawa kama lon li ken ala\",\"bold\":\"true\",\"obfuscated\":\"true\",\"color\":\"purple\"}"
data modify entity @s Item.tag.display.Lore append value "{\"text\":\"tenpo ni li tenpo monsuta\",\"bold\":\"true\",\"obfuscated\":\"true\",\"color\":\"purple\"}"

data modify entity @s Item.tag.CustomModelData set value 7106216



time set midnight
weather thunder



effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:wither 10 0 false
effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:nausea 20 0 false
effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:weakness 40 1 false
effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:slowness 30 1 false
effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:mining_fatigue 40 1 false
effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:hunger 30 1 false
effect give @e[distance=..40,gamemode=!creative,gamemode=!spectator] minecraft:darkness 10 0 false



fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:fire replace #xplsvtlts:lights
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:lava replace minecraft:water
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:coarse_dirt replace #xplsvtlts:killable_earth
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:rooted_dirt replace minecraft:moss_block
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:air replace #xplsvtlts:replaceable_plants
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:dead_bush replace #xplsvtlts:dead_bushable
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:bamboo_fence replace minecraft:bamboo
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:chorus_plant replace minecraft:cactus
fill ~-15 ~-15 ~-15 ~15 ~15 ~15 minecraft:pointed_dripstone[vertical_direction=down] replace minecraft:spore_blossom



particle minecraft:explosion_emitter ~ ~ ~ 0 0 0 0.0 1
playsound minecraft:entity.generic.explode hostile @a ~ ~ ~ 4.0 1.0



summon minecraft:armor_stand ~ ~ ~ {Tags:[xplsvtlts_error_error_invalid],Marker:1b,Invisible:1b}
scoreboard players set @e[type=minecraft:armor_stand,tag=xplsvtlts_error_error_invalid,limit=1,sort=nearest] xplsvtlts_error_error_error_error 14
schedule function xplsvtlts:reactive_plating/rorre/_error_loop_error 8t
