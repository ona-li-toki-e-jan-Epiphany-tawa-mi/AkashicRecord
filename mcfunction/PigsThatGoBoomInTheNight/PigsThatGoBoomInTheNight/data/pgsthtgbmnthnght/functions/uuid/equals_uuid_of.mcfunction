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
# Tests whether the given entity has the same UUID as that stored in [_uuid0, _uuid1, _uuid2, _uuid3] and stores the result in
#   _success.
#
# Needs to be called with @s being the entity.
# Make sure to call "scoreboard players reset _success pgsthtgbmnthnght" to reset the result after it is no longer needed.
#

execute store result score _comapare_uuid0 pgsthtgbmnthnght run data get entity @s UUID[0]
execute store result score _comapare_uuid1 pgsthtgbmnthnght run data get entity @s UUID[1]
execute store result score _comapare_uuid2 pgsthtgbmnthnght run data get entity @s UUID[2]
execute store result score _comapare_uuid3 pgsthtgbmnthnght run data get entity @s UUID[3]

scoreboard players set _success pgsthtgbmnthnght 1
execute unless score _uuid0 pgsthtgbmnthnght = _comapare_uuid0 pgsthtgbmnthnght run scoreboard players set _success pgsthtgbmnthnght 0
execute unless score _uuid1 pgsthtgbmnthnght = _comapare_uuid1 pgsthtgbmnthnght run scoreboard players set _success pgsthtgbmnthnght 0
execute unless score _uuid2 pgsthtgbmnthnght = _comapare_uuid2 pgsthtgbmnthnght run scoreboard players set _success pgsthtgbmnthnght 0
execute unless score _uuid3 pgsthtgbmnthnght = _comapare_uuid3 pgsthtgbmnthnght run scoreboard players set _success pgsthtgbmnthnght 0

scoreboard players reset _comapare_uuid0 pgsthtgbmnthnght
scoreboard players reset _comapare_uuid1 pgsthtgbmnthnght
scoreboard players reset _comapare_uuid2 pgsthtgbmnthnght
scoreboard players reset _comapare_uuid3 pgsthtgbmnthnght
