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
# Makes an item reactive as part of the reactive armor/tool crafting process.
#
# Parameters:
#   @s - the item entity to make reactive.
#
# Returns:
#   _made_reactive (scoreboard: xplsvtlts) - sets to true.
#

# Adds reactive identifiers to the item.
data modify entity @s Item.tag.xplsvtlts.Reactive set value 1b
execute unless predicate xplsvtlts:entity/tnt_wand/is_tnt_wand_item_entity run data modify entity @s Item.tag.display.Lore append value "{\"text\":\"Reactive\",\"italic\":false,\"color\":\"red\"}"

# Adds blast protection IV to armor if it is not already present (does not check
#   level.)
execute unless predicate xplsvtlts:entity/is_blast_protected_item_entity if predicate xplsvtlts:entity/is_armor_item_entity run data modify entity @s Item.tag.Enchantments append value {"id":"minecraft:blast_protection","lvl":4s}



#???
execute if predicate xplsvtlts:entity/tnt_wand/is_tnt_wand_item_entity run function xplsvtlts:reactive_plating/rorre/invalid



# Marks that the reative plating has been used up.
scoreboard players set _made_reactive xplsvtlts 1
