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
# Displays a projection of the remaining blocks needed to complete the
#   combustion forge multiblock structure.
#
# Make sure to call the function xplsvtlts:combustion_forge/projection/cleanup
#   afterwards to remove the projection entities.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#


execute positioned ^1 ^-0.75 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^-0.75 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^-0.75 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^-0.75 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^-0.75 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^-0.75 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^-0.75 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:target"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^-0.75 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^-0.75 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:target"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}



execute positioned ^1 ^0.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-95.0..-85.0] positioned ^ ^0.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:iron_trapdoor",Properties:{facing:"west",half:"top"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=85.0..95.0] positioned ^ ^0.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:iron_trapdoor",Properties:{facing:"east",half:"top"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-5.0..5.0] positioned ^ ^0.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:iron_trapdoor",Properties:{facing:"north",half:"top"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=175.0..185.0] positioned ^ ^0.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:iron_trapdoor",Properties:{facing:"south",half:"top"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^0.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^0.25 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^0.25 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^0.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-95.0..-85.0] positioned ^ ^0.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:dispenser",Properties:{facing:"east"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=85.0..95.0] positioned ^ ^0.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:dispenser",Properties:{facing:"west"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-5.0..5.0] positioned ^ ^0.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:dispenser",Properties:{facing:"south"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=175.0..185.0] positioned ^ ^0.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:dispenser",Properties:{facing:"north"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^0.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:obsidian"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}



execute positioned ^1 ^1.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^1.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^1.25 ^1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^1.25 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^1.25 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^1.25 ^ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^1.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^1.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^1.25 ^-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~ ~-0.25 {block_state:{Name:"minecraft:furnace"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_combustion_forge_projection"]}
