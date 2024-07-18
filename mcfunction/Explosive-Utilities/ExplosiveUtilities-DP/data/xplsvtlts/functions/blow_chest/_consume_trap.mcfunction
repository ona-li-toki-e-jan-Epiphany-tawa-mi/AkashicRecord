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
# Attempts to consume the trap in the blow chest.
#
# Parameters:
#   Location - at the blow chest.
#
# Returns:
#   _consumed_trap (scoreboard: xplsvtlts) - 1 if the trap was consumed, else 0.
#

scoreboard players set _consumed_trap xplsvtlts 0

execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:0b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.0 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:1b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.1 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:2b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.2 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:3b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.3 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:4b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.4 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:5b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.5 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:6b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.6 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:7b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.7 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:8b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.8 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:9b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.9 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:10b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.10 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:11b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.11 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:12b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.12 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:13b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.13 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:14b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.14 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:15b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.15 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:16b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.16 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:17b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.17 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:18b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.18 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:19b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.19 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:20b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.20 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:21b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.21 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:22b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.22 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:23b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.23 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:24b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.24 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:25b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.25 xplsvtlts:decrement_stack
execute if score _consumed_trap xplsvtlts matches 1 run return 0
execute store success score _consumed_trap xplsvtlts run execute if block ~ ~ ~ #xplsvtlts:blow_chest_trapable{Items:[{Slot:26b,id:"minecraft:enchanted_book",tag:{CustomModelData:14881210}}]} run item modify block ~ ~ ~ container.26 xplsvtlts:decrement_stack

