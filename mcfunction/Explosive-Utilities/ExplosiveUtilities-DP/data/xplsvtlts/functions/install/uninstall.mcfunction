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
# Uninstalls the pack.
#

scoreboard players reset installed xplsvtlts
scoreboard players reset tnt_wand_summon_cooldown xplsvtlts
scoreboard players reset tnt_wand_punch_cooldown xplsvtlts
scoreboard players reset reactive_armor_maximum_cooldown xplsvtlts
scoreboard players reset disable_tnt_wand_corraling_restrictions xplsvtlts
scoreboard players reset pistol_kiln_runtime xplsvtlts
scoreboard players reset pistol_kiln_gunpowder_fuel_time xplsvtlts
scoreboard players reset dynamite_fuse_time xplsvtlts
scoreboard players reset #2 xplsvtlts
scoreboard players reset #7 xplsvtlts
scoreboard objectives remove xplsvtlts

scoreboard players reset @e xplsvtlts_tnt_wand_summon_cooldown
scoreboard objectives remove xplsvtlts_error_error_error_error 
scoreboard players reset @a xplsvtlts_tnt_wand_summon_cooldown
scoreboard objectives remove xplsvtlts_tnt_wand_summon_cooldown
scoreboard players reset @a xplsvtlts_tnt_wand_punch_cooldown
scoreboard objectives remove xplsvtlts_tnt_wand_punch_cooldown
scoreboard players reset @e xplsvtlts_fuse_freeze_time
scoreboard objectives remove xplsvtlts_fuse_freeze_time
scoreboard players reset @a xplsvtlts_reactive_armor_cooldown
scoreboard objectives remove xplsvtlts_reactive_armor_cooldown
scoreboard players reset @e xplsvtlts_click_detector_uuid_memory_0
scoreboard objectives remove xplsvtlts_click_detector_uuid_memory_0
scoreboard players reset @e xplsvtlts_click_detector_uuid_memory_1
scoreboard objectives remove xplsvtlts_click_detector_uuid_memory_1
scoreboard players reset @e xplsvtlts_click_detector_uuid_memory_2
scoreboard objectives remove xplsvtlts_click_detector_uuid_memory_2
scoreboard players reset @e xplsvtlts_click_detector_uuid_memory_3
scoreboard objectives remove xplsvtlts_click_detector_uuid_memory_3
scoreboard players reset @e xplsvtlts_raycast_uuid_memory_0
scoreboard objectives remove xplsvtlts_raycast_uuid_memory_0
scoreboard players reset @e xplsvtlts_raycast_uuid_memory_1
scoreboard objectives remove xplsvtlts_raycast_uuid_memory_1
scoreboard players reset @e xplsvtlts_raycast_uuid_memory_2
scoreboard objectives remove xplsvtlts_raycast_uuid_memory_2
scoreboard players reset @e xplsvtlts_raycast_uuid_memory_3
scoreboard objectives remove xplsvtlts_raycast_uuid_memory_3
scoreboard players reset @e xplsvtlts_combustion_forge_runtime 
scoreboard objectives remove xplsvtlts_combustion_forge_runtime 
scoreboard players reset @e xplsvtlts_pistol_kiln_runtime 
scoreboard objectives remove xplsvtlts_pistol_kiln_runtime 
scoreboard players reset @e xplsvtlts_pistol_kiln_fuel_time 
scoreboard objectives remove xplsvtlts_pistol_kiln_fuel_time 
scoreboard players reset @e xplsvtlts_dynamite_fuse
scoreboard objectives remove xplsvtlts_dynamite_fuse 



tellraw @a {"text":"===================================================","color":"gold"}
tellraw @a {"text":"Successfully uninstalled Explosive Utilities!","color":"gold"}
tellraw @a {"text":"===================================================","color":"gold"}
