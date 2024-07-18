#Detects Golem Formation
execute as @a at @s if block ~ ~-1 ~ minecraft:carved_pumpkin if block ~ ~-2 ~ minecraft:brewing_stand if block ~ ~-3 ~ minecraft:brewing_stand run tag @s add sumBgolem
execute as @a at @s if block ~ ~-1 ~ minecraft:jack_o_lantern if block ~ ~-2 ~ minecraft:brewing_stand if block ~ ~-3 ~ minecraft:brewing_stand run tag @s add sumBgolem
#Clears Golem Structure
execute at @a[tag=sumBgolem] run fill ~ ~-1 ~ ~ ~-3 ~ air
#Summons Golem
execute at @a[tag=sumBgolem] run summon minecraft:snow_golem ~ ~-3 ~ {CustomNameVisible:1b,PersistenceRequired:1b,CustomName:"{\"text\":\"Fire Fighter Golem\",\"color\":\"dark_purple\"}"}
#Sounds And Particles For The Summon
execute at @a[tag=sumBgolem] run particle witch ~ ~-2.5 ~ 0 0 0 2 60 normal
execute at @a[tag=sumBgolem] run playsound minecraft:entity.splash_potion.break neutral @a ~ ~ ~ 1 0.8
#Clears Snow
execute at @e[type=snow_golem,name="Fire Fighter Golem"] run fill ~1 ~ ~1 ~-1 ~ ~-1 minecraft:air replace minecraft:snow
#Gives A Fire Resistance To The Golem
effect give @e[type=snow_golem,name="Fire Fighter Golem"] minecraft:fire_resistance 1 0 true
#Summons Water Potion
execute at @e[type=snow_golem,name="Fire Fighter Golem"] at @e[type=snowball,distance=..3] run summon potion ~ ~ ~ {NoGravity:1b,CustomNameVisible:0b,ownerName:"Brewing Golem",CustomName:"{\"text\":\"BGprojc\"}",Potion:{id:"minecraft:splash_potion",Count:1b,tag:{Potion:"minecraft:water"}}}
#Stores Snowball Motion Onto Bottle
execute as @e[name=BGprojc,tag=!SbAsum] at @s store result entity @s Motion[0] double 1 run data get entity @e[type=minecraft:snowball,distance=..1,limit=1] Motion[0] 
execute as @e[name=BGprojc,tag=!SbAsum] at @s store result entity @s Motion[1] double 1 run data get entity @e[type=minecraft:snowball,distance=..1,limit=1] Motion[1] 
execute as @e[name=BGprojc,tag=!SbAsum] at @s store result entity @s Motion[2] double 1 run data get entity @e[type=minecraft:snowball,distance=..1,limit=1] Motion[2] 
#Advancement
execute at @e[type=snow_golem,name="Fire Fighter Golem"] as @a[distance=..3] run advancement grant @s only voidpack1_13:firefightergolem
#Reset/Cleanup
tag @a remove sumBgolem
execute at @e[name=BGprojc,tag=!SbAsum] run kill @e[type=snowball,distance=..1,limit=1]
#Stop Storing Tag Applied
tag @e[name=BGprojc] add SbAsum