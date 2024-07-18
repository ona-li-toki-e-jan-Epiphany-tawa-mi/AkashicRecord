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
# Runs the structural validator, which test if the combustion forge is valid.
# Handles sending feedback to player when multiblock is completed or destoryed.
# Marks core as valid or invalid.
#
# Parameters:
#   @s - the combustion forge core.
#   Location - at @s.
#
# Returns:
#   Marks the core with the tag "xplsvtlts_was_valid_structure" if it is valid,
#       if not, the tag is removed.
#

execute store result score _is_valid_structure xplsvtlts run function xplsvtlts:combustion_forge/validation/_check_structure_validity

# Player completed multiblock.
execute if score _is_valid_structure xplsvtlts matches 1 if entity @s[tag=!xplsvtlts_was_valid_structure] run function xplsvtlts:combustion_forge/validation/_mark_as_valid
# Player broke completed multiblock.
execute if score _is_valid_structure xplsvtlts matches 0 if entity @s[tag=xplsvtlts_was_valid_structure] run function xplsvtlts:combustion_forge/validation/_mark_as_invalid

# Kickstarts looping for valid inactive combustion forges.
execute if score _is_valid_structure xplsvtlts matches 1 run schedule function xplsvtlts:combustion_forge/ideling/tick_inactive_cores 1s



scoreboard players reset _is_valid_structure xplsvtlts
