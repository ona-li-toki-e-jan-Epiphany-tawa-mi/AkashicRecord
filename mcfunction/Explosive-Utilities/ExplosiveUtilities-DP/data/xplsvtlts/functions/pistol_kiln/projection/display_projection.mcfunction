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
#   pistol kiln multiblock structure.
#
# Make sure to call the function xplsvtlts:pistol_kiln/projection/cleanup
#   afterwards to remove the projection entities.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

execute positioned ~1 ~-1 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~-1 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~-1 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~-1 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~-1 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:dispenser",Properties:{"facing":"up"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~-1 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~-1 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~-1 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~-1 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}



execute positioned ~1 ~ ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~ ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:blast_furnace",Properties:{"facing":"south"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~ ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~ ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:blast_furnace",Properties:{"facing":"east"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~ ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:blast_furnace",Properties:{"facing":"west"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~ ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~ ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:blast_furnace",Properties:{"facing":"north"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~ ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}



execute positioned ~1 ~1 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~1 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:hopper",Properties:{"facing":"down"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~1 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~1 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:hopper",Properties:{"facing":"down"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~1 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:hopper",Properties:{"facing":"down"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~1 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~1 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:hopper",Properties:{"facing":"down"}},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~1 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}



execute positioned ~1 ~2 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~2 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~2 ~1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~2 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~2 ~ if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~2 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~2 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~2 ~-1 if block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.25 ~0.25 ~-0.25 {block_state:{Name:"minecraft:polished_deepslate"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[0.5f,0.5f,0.5f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
