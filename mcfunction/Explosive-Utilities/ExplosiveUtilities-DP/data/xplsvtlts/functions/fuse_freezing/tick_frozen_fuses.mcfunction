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
# Runs once every tick whilst entities with a frozen fuse still exist.
# Needs to be kickstarted when an entity's fuse is frozen.
#
# Entities can be marked with the tag "xplsvtlts_gravity_on_thaw" so that their
#   gravity is restored upon thawing (meant for tnt wand.)
#

scoreboard players set _exists_players_with_cooldown xplsvtlts 0
execute store success score _exists_players_with_cooldown xplsvtlts run execute as @e[scores={xplsvtlts_fuse_freeze_time=1..}] run function xplsvtlts:fuse_freezing/_on_frozen_fuse_tick

execute if score _exists_players_with_cooldown xplsvtlts matches 1 run schedule function xplsvtlts:fuse_freezing/tick_frozen_fuses 1t

scoreboard players reset _exists_players_with_cooldown xplsvtlts
