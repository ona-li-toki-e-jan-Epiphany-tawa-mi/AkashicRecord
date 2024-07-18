#timer
scoreboard players add @e fallThrough 1
#Random Chance Generator For Players
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["falltSuccess"],CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @a[scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
kill @e[type=armor_stand,name=falltChance,limit=9,sort=random] 
#Applies Selector
execute at @e[type=armor_stand,name=falltChance,tag=falltSuccess] run tag @a[distance=..1] add willfallThrough
#Reset
kill @e[type=armor_stand,name=falltChance]
#The Fall Through Mekanism
execute at @a[tag=willfallThrough] as @a[tag=willfallThrough] run teleport @s ~ ~-1 ~
#Advancement
advancement grant @a[tag=willfallThrough] only darkathame_comp1_13:shittymcadvancements/worldfallthrough

#Clears Tag
execute at @a positioned ~ -3 ~ as @a[distance=..2] run tag @a[distance=..2] remove willfallThrough
#Random Chance Generator For Players
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["falltSuccess"],CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
execute at @e[limit=1,type=!player,scores={fallThrough=5400..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"falltChance\"}"}
kill @e[type=armor_stand,name=falltChance,limit=9,sort=random]
#Applies Selector
execute at @e[type=armor_stand,name=falltChance,tag=falltSuccess] run tag @e[type=!player,distance=..1] add willfallThrough
#Reset
kill @e[type=armor_stand,name=falltChance]
#The Fall Through Mekanism
execute at @e[type=!player,tag=willfallThrough] as @e[type=!player,tag=willfallThrough] run teleport @s ~ ~-1 ~

#Reset
scoreboard players set @e[scores={fallThrough=5400..}] fallThrough 0

