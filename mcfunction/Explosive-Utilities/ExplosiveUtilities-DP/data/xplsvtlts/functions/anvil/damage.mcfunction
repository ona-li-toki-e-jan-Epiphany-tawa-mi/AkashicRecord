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
# Damages an anvil, possibly destorying it.
#
# Parameters:
#   Location - the location of the anvil.
#

execute if block ~ ~ ~ minecraft:damaged_anvil run setblock ~ ~ ~ air 

execute if block ~ ~ ~ minecraft:chipped_anvil[facing=north] run setblock ~ ~ ~ damaged_anvil[facing=north]
execute if block ~ ~ ~ minecraft:chipped_anvil[facing=east] run setblock ~ ~ ~ damaged_anvil[facing=east]
execute if block ~ ~ ~ minecraft:chipped_anvil[facing=west] run setblock ~ ~ ~ damaged_anvil[facing=west]
execute if block ~ ~ ~ minecraft:chipped_anvil[facing=south] run setblock ~ ~ ~ damaged_anvil[facing=south]

execute if block ~ ~ ~ minecraft:anvil[facing=north] run setblock ~ ~ ~ chipped_anvil[facing=north]
execute if block ~ ~ ~ minecraft:anvil[facing=east] run setblock ~ ~ ~ chipped_anvil[facing=east]
execute if block ~ ~ ~ minecraft:anvil[facing=west] run setblock ~ ~ ~ chipped_anvil[facing=west]
execute if block ~ ~ ~ minecraft:anvil[facing=south] run setblock ~ ~ ~ chipped_anvil[facing=south]
