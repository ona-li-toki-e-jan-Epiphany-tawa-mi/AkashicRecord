##
# Attempts to summon a piece of tnt using the tnt wand.
# If the player is in survival, it will need to consume a piece of tnt to
#   succeed.
#
# Parameters:
#   @s - the player.
#   Location - where to summon the tnt.
#

scoreboard players set _summoned_tnt xplsvtlts 1
execute unless entity @s[gamemode=creative] run execute store success score _summoned_tnt xplsvtlts run clear @s minecraft:tnt 1

execute if score _summoned_tnt xplsvtlts matches 1 run function xplsvtlts:tnt_wand/summoning/_summon_tnt

scoreboard players reset _summoned_tnt xplsvtlts
