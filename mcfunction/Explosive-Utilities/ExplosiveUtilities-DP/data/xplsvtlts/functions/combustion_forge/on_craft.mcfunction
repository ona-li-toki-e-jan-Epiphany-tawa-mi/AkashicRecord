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
# Runs when a player crafts a combustion forge core.
#
# Parameters:
#   @s - the player.
#

recipe take @s xplsvtlts:crafting/combustion_forge_core
advancement revoke @s only xplsvtlts:event_listeners/on_craft/craft_combustion_forge_core
clear @s knowledge_book

give @s minecraft:armor_stand{CustomModelData:10462431,EntityTag:{Tags:["xplsvtlts_combustion_forge_core"],NoGravity:1b,Marker:1b,Small:1b,Invisible:1b,ArmorItems:[{},{},{},{id:"minecraft:armor_stand",Count:1b,tag:{CustomModelData:10462431}}]},display:{Name:'{"text":"Combustion Forge Core","italic":false}',Lore:['{"text":"Explosive Utilities","color":"blue","italic":false}','{"text":"Multiblock structure that uses explosions to craft items."}','{"text":"Place down for projection."}','{"text":"Faces player when placed."}','{"text":"Furnace orientation does not matter."}','{"text":"Crouch near core to pick up."}','{"text":"Close trapdoor to activate."}','{"text":"Consumes tnt from dispenser as fuel."}','{"text":"Can do up to 16 crafts per tnt."}','{"text":"Recipe ingredients are taken from furnaces."}','{"text":"Leftover ingredients will be ejected from the forge."}','{"text":"Left target outputs when running."}','{"text":"Right target outputs when done running."}','{"text":"Open trapdoor to finish."}','{"text":"Size: 3x3x3"}','{"text":"Bill of Materials:"}','{"text":"- 13x Obsidian"}','{"text":"- 9x Furnace"}','{"text":"- 2x Target"}','{"text":"- 1x Dispenser"}','{"text":"- 1x Iron Trapdoor"}']}}
