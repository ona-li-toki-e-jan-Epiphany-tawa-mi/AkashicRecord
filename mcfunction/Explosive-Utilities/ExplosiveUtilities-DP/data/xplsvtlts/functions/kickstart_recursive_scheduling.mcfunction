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
# Some ticking loops utilize recursive scheduling to only run when necessary.
# They shedule themselves if the thing they operate on (i.e. an entity) exists,
#   and stop when they are done.
# Sometimes these entities can unload, or players can log off, stopping the
#   loop prematurely.
# This function must be called every so often to ensure that any prematurely
#   stopped loops can restart if that entity is loaded or that player logs in 
#   again.
#
# Make sure to use the schedule command to run these functions so that they do 
#   not run twice.
#

schedule function xplsvtlts:click_detection/tick_click_detectors 1t
schedule function xplsvtlts:fuse_freezing/tick_frozen_fuses 1t
schedule function xplsvtlts:reactive_plating/armor/tick_cooldowns 1t
schedule function xplsvtlts:tnt_wand/cooldown/tick_punch_cooldowns 1t
schedule function xplsvtlts:tnt_wand/cooldown/tick_summon_cooldowns 1t
schedule function xplsvtlts:reactive_plating/rorre/_error_loop_error 1t
schedule function xplsvtlts:combustion_forge/processing/tick_active_cores 1t
schedule function xplsvtlts:pistol_kiln/processing/tick_active_cores 1t
schedule function xplsvtlts:dynamite/tick_dynamite 1t

schedule function xplsvtlts:combustion_forge/ideling/tick_inactive_cores 1s
schedule function xplsvtlts:pistol_kiln/ideling/tick_inactive_cores 1s 
