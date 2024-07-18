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
# Projects a red border around invalid blocks in the combustion forge
#   multiblock.
#
# Make sure to call the function xplsvtlts:combustion_forge/projection/cleanup
#   afterwards to remove the projection entities.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#

execute positioned ^1 ^-0.975 ^1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^-0.975 ^1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^-0.975 ^1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^-0.975 ^ unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^-0.975 ^ unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^-0.975 ^ unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^-0.975 ^-1 unless block ~ ~ ~ minecraft:target unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^-0.975 ^-1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^-0.975 ^-1 unless block ~ ~ ~ minecraft:target unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}



execute positioned ^1 ^0.025 ^1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-95.0..-85.0] positioned ^ ^0.025 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=west,half=top] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=85.0..95.0] positioned ^ ^0.025 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=east,half=top] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-5.0..5.0] positioned ^ ^0.025 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=north,half=top] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=175.0..185.0] positioned ^ ^0.025 ^1 unless block ~ ~ ~ minecraft:iron_trapdoor[facing=south,half=top] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^0.025 ^1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^0.025 ^ unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^0.025 ^ unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^0.025 ^ unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^0.025 ^-1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-95.0..-85.0] positioned ^ ^0.025 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=east] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=85.0..95.0] positioned ^ ^0.025 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=west] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=-5.0..5.0] positioned ^ ^0.025 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=south] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute if entity @s[y_rotation=175.0..185.0] positioned ^ ^0.025 ^-1 unless block ~ ~ ~ minecraft:dispenser[facing=north] unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^0.025 ^-1 unless block ~ ~ ~ minecraft:obsidian unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}



execute positioned ^1 ^1.025 ^1 unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^1.025 ^1 unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^1.025 ^1 unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^1.025 ^ unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^1.025 ^ unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^1.025 ^ unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}

execute positioned ^1 ^1.025 ^-1 unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^ ^1.025 ^-1 unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
execute positioned ^-1 ^1.025 ^-1 unless block ~ ~ ~ minecraft:furnace unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_combustion_forge_projection"]}
