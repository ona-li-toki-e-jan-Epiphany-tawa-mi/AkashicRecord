# Strongman strong
## Strongman stronger
effect give @e[team=strongman] minecraft:strength 1 3 true
## Strongman punch strong
effect give @e[team=strongman] minecraft:haste 1 2 true
## Strongman take little damage
effect give @e[team=strongman] minecraft:resistance 1 0 true
## Some speed buffs in weird situations, cuz stronge
execute as @e[team=strongman] at @s if block ~ ~ ~ minecraft:cobweb run effect give @s minecraft:speed 1 14 true

# Big muscles, big energy
## Detects if a player is doing an action worthy of massive hunger
execute as @a[team=strongman,scores={hungie1=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie2=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie3=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie4=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie5=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie6=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie7=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie8=1..}] run tag @s add hungE 
execute as @a[team=strongman,scores={hungie9=1..}] run tag @s add hungE 
## Applies hunger
execute as @a[team=strongman,tag=hungE] run effect give @s minecraft:hunger 1 3 true
## Optimized reset
execute if entity @a[team=strongman,tag=hungE] run function classes1_14:class/strongman1

# Too bawd you can'nt uze bowz cuz youws dumb az a bwox a' rokks
## Gives back arrows, cuzz I'm kind (also lets you steal arrows up close, unintentional, but pretty cool and it fits well)
execute at @e[team=strongman] as @e[type=arrow,distance=..2,scores={arrTi=..1}] run give @p minecraft:arrow 1
## Deletes arrows
execute at @e[team=strongman] as @e[type=arrow,distance=..2,scores={arrTi=..1}] run kill @s

#Lower xp
#Make dumb