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
# Runs on TNT item entities once every second.
#
# Needs to be called with @s being the item entity and located at @s.
#

# Locates the nearest non explosive pig and makes them explosive.
execute if score pigs_only pgsthtgbmnthnght matches 1 store success score _found_pig pgsthtgbmnthnght as @e[type=minecraft:pig,limit=1,sort=nearest,distance=..1,tag=!pgsthtgbmnthnght_explosive] run tag @s add pgsthtgbmnthnght_explosive

# If non-pigs can be chosen, locates the nearest entity, that isn't the thrower, and makes them explosive.
execute if score pigs_only pgsthtgbmnthnght matches 0 run function pgsthtgbmnthnght:uuid/get_thrower_uuid
execute if score pigs_only pgsthtgbmnthnght matches 0 as @e[type=!minecraft:item,limit=1,sort=nearest,distance=..1,tag=!pgsthtgbmnthnght_explosive] run function pgsthtgbmnthnght:tnt_item_entity/try_tag_generic
execute if score pigs_only pgsthtgbmnthnght matches 0 run function pgsthtgbmnthnght:uuid/reset_get_thrower_uuid

execute if score _found_pig pgsthtgbmnthnght matches 1 run function pgsthtgbmnthnght:tnt_item_entity/consume
scoreboard players reset _found_pig pgsthtgbmnthnght
