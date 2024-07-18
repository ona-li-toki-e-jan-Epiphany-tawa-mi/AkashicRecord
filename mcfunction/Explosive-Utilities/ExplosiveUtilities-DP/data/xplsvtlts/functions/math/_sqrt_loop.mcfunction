##################################################################################
# The code present here is a modified version of that from another project,      #
# <https://github.com/NOPEname/nn_math>, which is under the following license:   #
#--------------------------------------------------------------------------------#
# MIT License                                                                    #
#                                                                                #
# Copyright (c) 2020 NOPEname                                                    #
#                                                                                #
# Permission is hereby granted, free of charge, to any person obtaining a copy   #
# of this software and associated documentation files (the "Software"), to deal  #
# in the Software without restriction, including without limitation the rights   #
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      #
# copies of the Software, and to permit persons to whom the Software is          #
# furnished to do so, subject to the following conditions:                       #
#                                                                                #
# The above copyright notice and this permission notice shall be included in all #
# copies or substantial portions of the Software.                                #
#                                                                                #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  #
# SOFTWARE.                                                                      #
##################################################################################

##
# Inner loop for the function xplsvtlts:math/sqrt.
#
# Parameters:
#   _sqrt (scoreboard: xplsvtlts) - intialize to the number to get the square
#       root of.
#   _temp (scoreboard: xplsvtlts) - initialize to the number 1.
#
# Returns:
#   _sqrt (scoreboard: xplsvtlts) - the square root of _n.
#

scoreboard players operation _sqrt xplsvtlts += _temp xplsvtlts
scoreboard players operation _sqrt xplsvtlts /= #2 xplsvtlts
scoreboard players operation _temp xplsvtlts = _n xplsvtlts
scoreboard players operation _temp xplsvtlts /= _sqrt xplsvtlts

execute if score _sqrt xplsvtlts > _temp xplsvtlts run function xplsvtlts:math/_sqrt_loop
