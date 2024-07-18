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
# Attempts to consume 1 piece of gunpowder from the pistol kiln's fuel
#   container.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#
# Returns:
#   _consumed_fuel (scoreboard: xplsvtlts) - 1 if a piece of gunpowder
#       was consumed, else 0.
#

scoreboard players set _consumed_fuel xplsvtlts 0

execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:0b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.0 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:1b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.1 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:2b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.2 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:3b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.3 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:4b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.4 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:5b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.5 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:6b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.6 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:7b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.7 xplsvtlts:decrement_stack
execute if score _consumed_fuel xplsvtlts matches 1 run return 0
execute store success score _consumed_fuel xplsvtlts run execute if block ~ ~-1 ~ minecraft:dispenser{Items:[{Slot:8b,id:"minecraft:gunpowder"}]} run item modify block ~ ~-1 ~ container.8 xplsvtlts:decrement_stack
