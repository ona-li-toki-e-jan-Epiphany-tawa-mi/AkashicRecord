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
# Runs every tick for players who are holding a click-detection enabled item 
#   (see predicate xplsvtlts:is_holding_click_detection_enabled_item).
#
# Parameters:
#   @s - the player.
#   Location - at @s.
#

function xplsvtlts:uuid/get_uuid
# Players' UUIDs need to be stored inside the memory for the click detectors
#   to compare with when finding their owner.
function xplsvtlts:click_detection/store_uuid_into_click_detector_memory

execute unless entity @e[limit=1,type=minecraft:interaction,tag=xplsvtlts_click_detector,predicate=xplsvtlts:entity/does_uuid_match_click_detector_memory] summon minecraft:interaction run function xplsvtlts:click_detection/_create_click_detector

function xplsvtlts:uuid/reset_get_uuid



# Kickstarts click detector tick loop (no problem if it's already running, idempotent and all.)
schedule function xplsvtlts:click_detection/tick_click_detectors 1t
