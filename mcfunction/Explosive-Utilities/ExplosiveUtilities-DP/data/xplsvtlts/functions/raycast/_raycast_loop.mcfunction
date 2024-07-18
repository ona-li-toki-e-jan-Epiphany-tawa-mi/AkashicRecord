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
# Runs the raycast.
#
# If there is a source entity, they will need to be loaded into the raycast
#   memory.
#
# Parameters:
#   @s - the raycast marker as a fresh armor stand.
#   Location - at @s.
#   _max_distance (scoreboard: xplsvtlts) - the remaning distance before ending
#       the raycast.
#   _collide_with_entities (scoreboard: xplsvtlts) - 1 to collide with entities,
#       0 to not.
#   [_uuid0, _uuid1, _uuid2, _uuid3] (scoreboard: xplsvtlts) - the UUID of the
#       source entity, if applicable.
#
# Returns:
#   An armor stand with the tag "xplsvtlts_raycast_marker" as the raycast
#       result.
#

execute if score _max_distance xplsvtlts matches ..0 run return 0
execute unless block ~ ~ ~ #replaceable run return 0
execute if score _collide_with_entities xplsvtlts matches 1 if entity @e[limit=1,distance=..1.1,sort=nearest,predicate=!xplsvtlts:entity/is_ignorable,predicate=!xplsvtlts:entity/does_uuid_match_raycast_memory] run return 0

tp @s ^ ^ ^1
scoreboard players remove _max_distance xplsvtlts 1
execute at @s run function xplsvtlts:raycast/_raycast_loop
