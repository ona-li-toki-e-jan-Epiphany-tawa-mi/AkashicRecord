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
# Attempts to tag a generic entity as explosive, failing if they are the thrower of the tnt.
# Sets _found_pig to be whether the entity was given the tag.
#
# Needs to be called with @s the entity. Ensure it is not an item entity, or atleast not the tnt item entity.
# Must call "function pgsthtgbmnthnght:uuid/get_thrower_uuid" prior to using this to exclude the thrower.
#

execute if score _has_thrower pgsthtgbmnthnght matches 1 run function pgsthtgbmnthnght:uuid/equals_uuid_of
execute if score _has_thrower pgsthtgbmnthnght matches 0 run scoreboard players set _success pgsthtgbmnthnght 0

execute if score _success pgsthtgbmnthnght matches 0 run tag @s add pgsthtgbmnthnght_explosive
execute if score _success pgsthtgbmnthnght matches 0 run scoreboard players set _found_pig pgsthtgbmnthnght 1

scoreboard players reset _success pgsthtgbmnthnght
