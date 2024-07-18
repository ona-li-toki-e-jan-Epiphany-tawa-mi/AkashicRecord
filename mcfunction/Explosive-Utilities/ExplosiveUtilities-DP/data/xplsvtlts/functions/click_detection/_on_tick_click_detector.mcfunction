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
# Runs every tick for click detectors.
#
# Parameters:
#   @s - the click detector.
#   Locations - at @s.
#

# Preperation for finding the right player using the predicate
#   xplsvtlts:entity/does_uuid_match_click_detector_memory.
function xplsvtlts:click_detection/load_uuid_from_click_detector_memory
# Ensures the detector stays around the player.
scoreboard players set _found_player xplsvtlts 0
execute store success score _found_player xplsvtlts run tp @s @a[limit=1,predicate=xplsvtlts:entity/does_uuid_match_click_detector_memory,predicate=xplsvtlts:entity/is_holding_click_detection_enabled_item]

# We can just kill it if it got too far away or the player is no longer needing
#   detection.
execute if score _found_player xplsvtlts matches 0 run kill @s



scoreboard players reset _found_player xplsvtlts
function xplsvtlts:uuid/reset_get_uuid
