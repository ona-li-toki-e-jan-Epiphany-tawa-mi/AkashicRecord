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
# Sets the owner of a projectile entity to that stored in [_uuid0, _uuid1, _uuid2, _uuid3].
#
# Needs to be called with @s being the projectile.
#

# Set owner to dummy value to make sure that the tag exists.
data merge entity @s {Owner:[I;0,0,0,0]}

execute store result entity @s Owner[0] int 1.0 run scoreboard players get _uuid0 incndrybs 
execute store result entity @s Owner[1] int 1.0 run scoreboard players get _uuid1 incndrybs
execute store result entity @s Owner[2] int 1.0 run scoreboard players get _uuid2 incndrybs
execute store result entity @s Owner[3] int 1.0 run scoreboard players get _uuid3 incndrybs
