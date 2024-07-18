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
# Creates the explosion and and a maker used to create the knockback effect.
# The supplied creeper is used to make the explosion and mark the location to
#   source the knockback from.
#
# Parameters:
#   @s - the creeper.
#   Location - at @s.
#

# Modifies the creeper to make the actual explosion. The explosion power is
#   toned down so the player doesn't Minecraft themselves.
data merge entity @s {ExplosionRadius:2b,Fuse:0s,"CustomName":'{"text":"Explosion Wand Explosive Punch"}',CustomNameVisible:false}

summon minecraft:armor_stand ~ ~ ~ {Marker:1b,Invisible:1b,Tags:["xplsvtlts_punch_knockback_marker"]}
schedule function xplsvtlts:tnt_wand/punch/_apply_knock_back 1t
