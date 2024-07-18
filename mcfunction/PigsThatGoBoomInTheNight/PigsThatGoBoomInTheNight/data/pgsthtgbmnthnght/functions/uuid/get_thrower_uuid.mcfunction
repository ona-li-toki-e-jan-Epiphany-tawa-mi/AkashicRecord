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
# Gets the UUID of the thrower of an item entity.
# _has_thrower will store whether the item entity has a thrower.
# [_uuid0, _uuid1, _uuid2, _uuid3] will store the thrower's UUID. Defaults to 0 if there is no thrower.
#
# Needs to be called with @s being the item entity.
# Make sure to call "function pgsthtgbmnthnght:uuid/reset_get_thrower_uuid" to reset the results after they are no longer 
#   needed.

execute store success score _has_thrower pgsthtgbmnthnght run data get entity @s Thrower

execute store result score _uuid0 pgsthtgbmnthnght run data get entity @s Thrower[0]
execute store result score _uuid1 pgsthtgbmnthnght run data get entity @s Thrower[1]
execute store result score _uuid2 pgsthtgbmnthnght run data get entity @s Thrower[2]
execute store result score _uuid3 pgsthtgbmnthnght run data get entity @s Thrower[3]
