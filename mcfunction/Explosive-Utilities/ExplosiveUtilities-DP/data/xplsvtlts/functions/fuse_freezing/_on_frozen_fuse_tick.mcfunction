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
# Runs every tick on entities with a frozen fuse.
# Increments the fuse of an explosive entity to prevent it from exploding.
#
# Parameters:
#   @s - the entity with a frozen fuse.
#

execute store result score _fuse xplsvtlts run data get entity @s Fuse
# The conditional here just makes sure that we don't make a stupidly long fuse.
execute if score _fuse xplsvtlts matches ..80 run scoreboard players add _fuse xplsvtlts 1
execute store result entity @s Fuse short 1.0 run scoreboard players get _fuse xplsvtlts

scoreboard players reset _fuse xplsvtlts



scoreboard players remove @s xplsvtlts_fuse_freeze_time 1

execute if score @s xplsvtlts_fuse_freeze_time matches ..0 run function xplsvtlts:fuse_freezing/_on_fuse_thaw
