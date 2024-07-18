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
# Normalizes the given vector.
#
# Parameters:
#   Vector register 1 - the vector to normalize.
#
# Returns:
#   Vector register 1 - the normalized vector.
#

function xplsvtlts:vector3d/get_magnitude

# The magnitude is 70 times what it should be because we need to capture the
#   decimal component, so we need to multiply the vector by 70 to cancel it out.
scoreboard players set _scalar xplsvtlts 70
function xplsvtlts:vector3d/scalar_multiply

scoreboard players operation _x1 xplsvtlts /= _magnitude xplsvtlts
scoreboard players operation _y1 xplsvtlts /= _magnitude xplsvtlts
scoreboard players operation _z1 xplsvtlts /= _magnitude xplsvtlts

scoreboard players reset _magnitude xplsvtlts
scoreboard players reset _scalar
