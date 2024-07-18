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
# Tests whether the given entity has the same UUID as the given UUID and
#   stores the result.
#
# Parameters:
#   @s - the entity to get the UUID from.
#   [_uuid0, _uuid1, _uuid2, _uuid3] (scoreboard: xplsvtlts) - the UUID to
#       compare with.
#
# Returns:
#   _success (scoreboard: xplsvtlts) - whether the UUIDs are the same
#

execute store result score _comapare_uuid0 xplsvtlts run data get entity @s UUID[0]
execute store result score _comapare_uuid1 xplsvtlts run data get entity @s UUID[1]
execute store result score _comapare_uuid2 xplsvtlts run data get entity @s UUID[2]
execute store result score _comapare_uuid3 xplsvtlts run data get entity @s UUID[3]

scoreboard players set _success xplsvtlts 1
execute unless score _uuid0 xplsvtlts = _comapare_uuid0 xplsvtlts run scoreboard players set _success xplsvtlts 0
execute unless score _uuid1 xplsvtlts = _comapare_uuid1 xplsvtlts run scoreboard players set _success xplsvtlts 0
execute unless score _uuid2 xplsvtlts = _comapare_uuid2 xplsvtlts run scoreboard players set _success xplsvtlts 0
execute unless score _uuid3 xplsvtlts = _comapare_uuid3 xplsvtlts run scoreboard players set _success xplsvtlts 0

scoreboard players reset _comapare_uuid0 xplsvtlts
scoreboard players reset _comapare_uuid1 xplsvtlts
scoreboard players reset _comapare_uuid2 xplsvtlts
scoreboard players reset _comapare_uuid3 xplsvtlts
