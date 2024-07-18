# Imports when the scoreboards are nonexistent
## Creates teams
execute unless entity @e[type=aromor_stand,name=initT] run team add scripter
execute unless entity @e[type=aromor_stand,name=initT] run team add strongman
## Various scoreboards used for commands
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add arrTi dummy
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add foLv dummy
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add foLv2 dummy
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie1 minecraft.custom:minecraft.climb_one_cm
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie2 minecraft.custom:minecraft.damage_dealt
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie3 minecraft.custom:minecraft.jump
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie4 minecraft.custom:minecraft.sprint_one_cm
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie5 minecraft.custom:minecraft.swim_one_cm
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie6 minecraft.custom:minecraft.damage_blocked_by_shield
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie7 minecraft.custom:minecraft.walk_on_water_one_cm
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie8 minecraft.custom:minecraft.walk_one_cm
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add hungie9 minecraft.custom:minecraft.walk_under_water_one_cm
## Declares constants
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard objectives add const dummy
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard players set 2 const 2
execute unless entity @e[type=aromor_stand,name=initT] run scoreboard players set 1 const 1

# Removes scoreboards whilst they are existent
## Disbands teams
execute if entity @e[type=aromor_stand,name=initT] run team remove scripter
execute if entity @e[type=aromor_stand,name=initT] run team remove strongman
## Removes various scoreboards
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove arrTi
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove foLv
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove foLv2
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie1
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie2
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie3
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie4
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie5
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie6
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie7
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie8
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove hungie9
## Uproots constants
execute if entity @e[type=aromor_stand,name=initT] run scoreboard objectives remove const

# Checker
## Used to signify that stuff has been removed
execute as @e[type=aromor_stand,name=initT] if entity @s run kill @s
## Used to signify that stuff has been imported
execute at @s unless entity @e[type=aromor_stand,name=initT] run summon armor_stand ~ ~ ~ {NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"initT\"}"}
