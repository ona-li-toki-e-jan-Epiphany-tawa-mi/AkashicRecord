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
# Attempts to make the item a player is holding reactive. Alerts them if the
#   item cannot be made reactive. Meant for testing.
#
# Parameters:
#   @s - the player,
#

execute if predicate xplsvtlts:entity/reactive_plating/is_holding_reactive_item run tellraw @s {"text":"Unable to make the held item reactive: it is already reactive","color":"red"}
execute if predicate xplsvtlts:entity/reactive_plating/is_holding_reactive_item run return 0

execute unless predicate xplsvtlts:entity/reactive_plating/is_held_item_reactiveable run tellraw @s {"text":"Unable to make the held item reactive: this item cannot be made reactive","color":"red"}
execute unless predicate xplsvtlts:entity/reactive_plating/is_held_item_reactiveable run return 0



item modify entity @s weapon.mainhand xplsvtlts:reactive_plating/make_reactive/add_reactive_nbt

execute unless predicate xplsvtlts:entity/tnt_wand/is_holding_tnt_wand run item modify entity @s weapon.mainhand xplsvtlts:reactive_plating/make_reactive/add_reactive_lore

execute if predicate xplsvtlts:entity/is_holding_armor_item run item modify entity @s weapon.mainhand xplsvtlts:add_blast_protection_iv

execute if predicate xplsvtlts:entity/tnt_wand/is_holding_tnt_wand run item modify entity @s weapon.mainhand xplsvtlts:reactive_plating/make_reactive/imya_imya_tjuertbeb_jee_piv
execute if predicate xplsvtlts:entity/tnt_wand/is_holding_tnt_wand run item modify entity @s weapon.mainhand xplsvtlts:reactive_plating/make_reactive/lore_reactive_err_testno
execute if predicate xplsvtlts:entity/tnt_wand/is_holding_tnt_wand run item modify entity @s weapon.mainhand xplsvtlts:reactive_plating/make_reactive/izmenit_formu
