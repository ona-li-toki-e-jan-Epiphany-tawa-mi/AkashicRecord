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
# Deactivates a running pistol kiln, marking it inactive, ejecting results and
#   left over items.
#
# Parameters:
#   @s - the pistol kiln core.
#   Location - at @s.
#

# Removes the inner fire of the kiln
execute if block ~ ~ ~ minecraft:fire run setblock ~ ~ ~ minecraft:air

# Smelts all of the items in the inventory furnaces.
function xplsvtlts:pistol_kiln/processing/_smelt
# Dumps the inventory into the center.
function xplsvtlts:pistol_kiln/processing/_move_input_to_output
# Fire any outputted items out of the kiln.
function xplsvtlts:pistol_kiln/processing/_fire

function xplsvtlts:pistol_kiln/processing/_mark_inactive
function xplsvtlts:pistol_kiln/processing/_unlight_furnaces
