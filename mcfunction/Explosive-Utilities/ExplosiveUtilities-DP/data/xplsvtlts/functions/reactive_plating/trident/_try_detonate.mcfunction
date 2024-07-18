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
# Attempts to detonate the reactive trident thrown by the player.
#
# Parameters:
#   @s - the reactive trident.
#   [_uuid0, _uuid1, _uuid2, _uuid3] (scoreboard: xplsvtlts) - the UUID of the
#       player who threw it.
#   _success (scoreboard: xplsvtlts) - intialize to 0. Used to check to see if a
#       trident was found
#
# Returns:
#   _success (scoreboard: xplsvtlts) - 2 if a trident was detonated, else 0 or
#       1.
#

execute if score _success xplsvtlts matches 0 run function xplsvtlts:uuid/equals_uuid_of_owner

execute if score _success xplsvtlts matches 1 run function xplsvtlts:reactive_plating/trident/_detonate
# Prevents further tridents from exploding from 1 hit.
execute if score _success xplsvtlts matches 1 run scoreboard players set _success xplsvtlts 2
