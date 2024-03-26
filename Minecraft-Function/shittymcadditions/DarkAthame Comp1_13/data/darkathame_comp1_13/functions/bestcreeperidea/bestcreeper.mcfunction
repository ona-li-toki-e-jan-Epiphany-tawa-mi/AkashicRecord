#Detecting Hurt Creepers
scoreboard players add @e[type=creeper] creeperDamage 1 
scoreboard players set @e[type=creeper,nbt={HurtTime:0s}] creeperDamage 0
#Arrow Detection
execute at @e[type=arrow,tag=!creeperTagged] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperExplode\"}"}
execute at @e[type=armor_stand,name=creeperExplode] as @e[type=arrow,distance=..1,nbt={inGround:0b}] run tag @s add creeperTagged
execute as @e[type=armor_stand,name=creeperExplode] at @s run teleport @s @e[type=arrow,sort=nearest,limit=1,distance=..4,nbt={inGround:0b}]
#Explosion
execute at @e[type=armor_stand,name=creeperExplode] unless entity @e[type=arrow,distance=..2] as @e[type=creeper,distance=..3,tag=!Cdefused] run data merge entity @s {Fuse:0}  
#Reset
execute as @e[type=armor_stand,name=creeperExplode] at @s unless entity @e[type=arrow,distance=..2,nbt={inGround:0b}] run kill @e[type=armor_stand,name=creeperExplode]
scoreboard players add @e[type=armor_stand,name=creeperExplode] creeperDamage 1
kill @e[type=armor_stand,name=creeperExplode,scores={creeperDamage=1200..}]
#Detects A Player Hitting A Creeper
execute at @a[scores={attackCreeper=1..}] as @e[type=creeper,distance=..6,scores={creeperDamage=1..},tag=!Cdefused] run tag @s add explodeChance
#Explode Randomizer
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["CwillNotexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwillexplode"],CustomName:"{\"text\":\"creeperEChance\"}"}
execute at @e[type=creeper,tag=explodeChance] run kill @e[name=creeperEChance,limit=9,sort=random,distance=..1] 
#90% Explode Chance (9/10)
execute at @e[type=armor_stand,name=creeperEChance,tag=Cwillexplode] as @e[type=creeper,distance=..1] run data merge entity @s {Fuse:0}
#Check For No Explosion
execute at @e[type=armor_stand,name=creeperEChance,tag=CwillNotexplode] as @e[type=creeper,tag=explodeChance,distance=..1] if entity @a[limit=1,distance=..10,nbt={SelectedItem:{id:"minecraft:shears"}}] run tag @s add defuseChance
#Cleanup
kill @e[type=armor_stand,name=creeperEChance]
#Defuse Randomizer
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,Tags:["Cwilldefuse"],CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Invisible:1b,Marker:1b,CustomName:"{\"text\":\"creeperDChance\"}"}
execute as @e[type=creeper,tag=explodeChance] at @s[tag=defuseChance] run kill @e[type=armor_stand,name=creeperDChance,limit=9,sort=random,distance=..1]
#1% Defuse Chace (1/100)
execute at @e[type=armor_stand,name=creeperDChance,tag=Cwilldefuse] run tag @e[type=creeper,distance=..1] add Cdefused
#Cleanup
kill @e[type=armor_stand,name=creeperDChance]
#Cleanup & Reset
tag @e remove defuseChance
tag @e remove explodeChance
scoreboard players set @a attackCreeper 0
