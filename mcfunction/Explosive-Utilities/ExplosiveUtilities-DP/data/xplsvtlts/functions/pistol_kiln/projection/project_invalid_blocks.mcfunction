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
# Projects a red border around invalid blocks in the pistol kiln multiblock.
#
# Make sure to call the function xplsvtlts:pistol_kiln/projection/cleanup
#   afterwards to remove the projection entities.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#
#

execute positioned ~1 ~-1 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~-1 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~-1 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~-1 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~-1 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:dispenser[facing=up] run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~-1 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~-1 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~-1 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~-1 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}



execute positioned ~1 ~ ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~ ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:blast_furnace[facing=south] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~ ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~ ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:blast_furnace[facing=east] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~ ~ ~ unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~ ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:blast_furnace[facing=west] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~ ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~ ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:blast_furnace[facing=north] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~ ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}



execute positioned ~1 ~1 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~1 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:hopper[facing=down] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~1 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~1 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:hopper[facing=down] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~ ~1 ~ unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~1 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:hopper[facing=down] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~1 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~1 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ minecraft:hopper[facing=down] unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~1 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}



execute positioned ~1 ~2 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~2 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~2 ~1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~2 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~ ~2 ~ unless block ~ ~ ~ #minecraft:replaceable run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~2 ~ unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}

execute positioned ~1 ~2 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~0 ~2 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
execute positioned ~-1 ~2 ~-1 unless block ~ ~ ~ #minecraft:replaceable unless block ~ ~ ~ #xplsvtlts:pistol_kiln_wall_materials run summon minecraft:block_display ~-0.525 ~ ~-0.525 {block_state:{Name:"minecraft:red_stained_glass"},transformation:{left_rotation:[0f,0f,0f,1f],right_rotation:[0f,0f,0f,1f],translation:[0f,0f,0f],scale:[1.05f,1.05f,1.05f]},Tags:["xplsvtlts_pistol_kiln_projection"]}
