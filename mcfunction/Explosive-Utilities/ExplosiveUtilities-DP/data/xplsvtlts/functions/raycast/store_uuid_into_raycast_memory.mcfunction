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
# Stores the UUID stored under the given entity in the raycast memory.
#
# Players use this memory for storing their UUID for later comparison.
#
# Parameters:
#   @s - the entity to store the UUID under.
#   [_uuid0, _uuid1, _uuid2, _uuid3] (scoreboard: xplsvtlts) - the UUID to
#       store.
#

scoreboard players operation @s xplsvtlts_raycast_uuid_memory_0 = _uuid0 xplsvtlts
scoreboard players operation @s xplsvtlts_raycast_uuid_memory_1 = _uuid1 xplsvtlts
scoreboard players operation @s xplsvtlts_raycast_uuid_memory_2 = _uuid2 xplsvtlts
scoreboard players operation @s xplsvtlts_raycast_uuid_memory_3 = _uuid3 xplsvtlts
