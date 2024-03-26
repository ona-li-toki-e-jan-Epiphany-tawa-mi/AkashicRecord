#Stores Daytime And Fire Time Of Creepers As A Score
execute store result score @e[type=creeper] creeperTime run time query daytime
execute as @e[type=creeper] store result score @s creeperFire run data get entity @s Fire 1
#Tests If A Creeper Is In Sunlight At Day
execute as @e[type=creeper] at @s unless entity @s[scores={creeperTime=12567..23450}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"crepBurn\"}"}
execute at @e[type=creeper] as @e[type=armor_stand,name=crepBurn,distance=..1] run spreadplayers ~ ~ 0 1 false @s
#Burns Said Creeper
execute at @e[type=armor_stand,name=crepBurn] as @e[type=creeper,distance=..1] run data merge entity @s {Fire:240s}
#Explosion For Any Creepers On Fire
execute as @e[type=creeper,scores={creeperFire=1..}] run data merge entity @s {ignited:1b}
#Reset
kill @e[type=armor_stand,name=crepBurn]