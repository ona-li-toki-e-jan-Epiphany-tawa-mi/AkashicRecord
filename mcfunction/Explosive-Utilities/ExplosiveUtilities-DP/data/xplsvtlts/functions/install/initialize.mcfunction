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
# Runs the installation process.
# Do not call directly, but through xplsvtlts:try_install, as it only runs when
#   not already installed (unless you want to reset the variables.)
#

# Create scoreboard in case of force-install and scoreboard does not exist for
#   some reason.
scoreboard objectives add xplsvtlts dummy
# Flags that installation has occured.
scoreboard players set installed xplsvtlts 1
# The cooldown time, in ticks, between when tnt can be summoned with a tnt wand.
scoreboard players set tnt_wand_summon_cooldown xplsvtlts 40
# The cooldown time, in ticks, between when an explosive punch can be done with
#   a tnt wand.
scoreboard players set tnt_wand_punch_cooldown xplsvtlts 80
# The maximum cooldown time, in ticks, until reactive armor can explode again.
scoreboard players set reactive_armor_maximum_cooldown xplsvtlts 120
# Whether to disable the restriction on which entities can be corraled with the
#   tnt wand.
scoreboard players set disable_tnt_wand_corraling_restrictions xplsvtlts 0
# The time it takes for the pistol kiln to smelt items
scoreboard players set pistol_kiln_runtime xplsvtlts 9000
# The fuel time, in ticks, that each piece of gunpowder provides.
scoreboard players set pistol_kiln_gunpowder_fuel_time xplsvtlts 800
# The time of the fuse on dynamite, in ticks.
scoreboard players set dynamite_fuse_time xplsvtlts 60
# Constants.
scoreboard players set #2 xplsvtlts 2
scoreboard players set #7 xplsvtlts 7

scoreboard objectives add xplsvtlts_error_error_error_error dummy
# A cool down for summoning tnt with the tnt wand.
scoreboard objectives add xplsvtlts_tnt_wand_summon_cooldown dummy
# A cool down for punching with the tnt wand.
scoreboard objectives add xplsvtlts_tnt_wand_punch_cooldown dummy
# The amount of time a piece of tnt has before it's fuse can continue.
scoreboard objectives add xplsvtlts_fuse_freeze_time dummy
# A cooldown for the explosion ability of reactive armor.
scoreboard objectives add xplsvtlts_reactive_armor_cooldown dummy
# Used to store the uuid of players and the owner of a click detector for
#   comparison.
scoreboard objectives add xplsvtlts_click_detector_uuid_memory_0 dummy
scoreboard objectives add xplsvtlts_click_detector_uuid_memory_1 dummy
scoreboard objectives add xplsvtlts_click_detector_uuid_memory_2 dummy
scoreboard objectives add xplsvtlts_click_detector_uuid_memory_3 dummy
# Used to store the uuid of entities being raycasted from for comparison.
scoreboard objectives add xplsvtlts_raycast_uuid_memory_0 dummy
scoreboard objectives add xplsvtlts_raycast_uuid_memory_1 dummy
scoreboard objectives add xplsvtlts_raycast_uuid_memory_2 dummy
scoreboard objectives add xplsvtlts_raycast_uuid_memory_3 dummy
# Used to track how long combustion forges have been running for, in ticks.
scoreboard objectives add xplsvtlts_combustion_forge_runtime dummy
# Used to track how long pistol kilns have been running for, in ticks.
scoreboard objectives add xplsvtlts_pistol_kiln_runtime dummy
# Used to track how long until pistol kilns need to consume more fuel.
scoreboard objectives add xplsvtlts_pistol_kiln_fuel_time dummy
# Used to track the fuses of dynamite entites.
scoreboard objectives add xplsvtlts_dynamite_fuse dummy


tellraw @a {"text":"===================================================","color":"gold"}
tellraw @a {"text":"Successfully installed Explosive Utilities!","color":"gold"}
tellraw @a [{"text":"To uninstall, run '","color":"gold"},{"text":"/function xplsvtlts:uninstall","color":"white","bold":true,"clickEvent":{"action":"run_command","value":"/function xplsvtlts:install/uninstall"},"hoverEvent":{"action":"show_text","value":{"text":"Click to run command","italic":true}}},{"text":"' and remove it from the world's datapack directory"}]
tellraw @a {"text":"===================================================","color":"gold"}
