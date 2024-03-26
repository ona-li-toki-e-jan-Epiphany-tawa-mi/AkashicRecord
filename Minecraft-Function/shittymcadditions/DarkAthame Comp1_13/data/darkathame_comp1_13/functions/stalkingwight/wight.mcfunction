#Gets Rid Of The Phantoms On Day 10
execute at @a[scores={wightRestless=240000..}] as @e[type=phantom,distance=..50] run kill @s
#Stores Time Of Day
execute store result score @a[scores={wightRestless=288000..}] wightAwake run time query daytime
#Summons Wight
execute at @a[scores={wightRestless=288000..}] unless entity @e[type=stray,name=Wight,distance=..50] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"wightSummon\"}"}
execute as @e[type=armor_stand,name=wightSummon] at @s run spreadplayers ~ ~ 5 50 false @s 
execute as @a[scores={wightRestless=288000..359999,wightAwake=12567..24000}] at @e[type=armor_stand,name=wightSummon] run summon skeleton_horse ~ ~ ~ {Silent:1b,Invulnerable:0b,CustomNameVisible:0b,DeathLootTable:"null",PersistenceRequired:1b,Tame:1b,SkeletonTrap:0b,Passengers:[{id:"minecraft:stray",Silent:1b,Invulnerable:0b,CustomNameVisible:1b,DeathLootTable:"null",PersistenceRequired:1b,CustomName:"{\"text\":\"Wight\",\"color\":\"aqua\",\"bold\":true,\"strikethrough\":true}",HandItems:[{id:"minecraft:diamond_sword",Count:1b},{id:"minecraft:diamond_sword",Count:1b}],HandDropChances:[0.0F,0.0F],ArmorItems:[{},{},{},{id:"minecraft:carved_pumpkin",Count:1b}],ArmorDropChances:[0.85F,0.85F,0.85F,0.0F],Attributes:[{Name:generic.followRange,Base:0},{Name:generic.attackDamage,Base:2147483647}]}],CustomName:"{\"text\":\"Wight Horse\"}",ArmorItems:[{},{},{},{}],ArmorDropChances:[0.85F,0.85F,0.0F,0.85F],Attributes:[{Name:generic.movementSpeed,Base:3},{Name:horse.jumpStrength,Base:1}]}
execute as @a[scores={wightRestless=360000..}] at @e[type=armor_stand,name=wightSummon] run summon skeleton_horse ~ ~ ~ {Silent:0b,Invulnerable:0b,CustomNameVisible:0b,DeathLootTable:"null",PersistenceRequired:1b,Tame:1b,SkeletonTrap:0b,Passengers:[{id:"minecraft:stray",Silent:0b,Invulnerable:0b,CustomNameVisible:1b,DeathLootTable:"null",PersistenceRequired:1b,CustomName:"{\"text\":\"Wight\",\"color\":\"aqua\",\"bold\":true,\"strikethrough\":true}",HandItems:[{id:"minecraft:diamond_sword",Count:1b},{id:"minecraft:diamond_sword",Count:1b}],HandDropChances:[0.0F,0.0F],ArmorItems:[{},{},{},{id:"minecraft:carved_pumpkin",Count:1b}],ArmorDropChances:[0.85F,0.85F,0.85F,0.0F],Attributes:[{Name:generic.followRange,Base:80},{Name:generic.attackDamage,Base:2147483647}]}],CustomName:"{\"text\":\"Wight Horse\"}",ArmorItems:[{},{},{},{}],ArmorDropChances:[0.85F,0.85F,0.0F,0.85F],Attributes:[{Name:generic.movementSpeed,Base:3},{Name:horse.jumpStrength,Base:1}]}
#Clean Up
kill @e[type=armor_stand,name=wightSummon]
#Forces Wight To Follow Player 
execute as @e[type=stray,name=Wight] at @s unless entity @a[distance=..50,scores={wightRestless=288000..},sort=nearest] run kill @s
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s unless entity @a[distance=..50,scores={wightRestless=288000..},sort=nearest] run kill @s
#12-14 Wights Disappear Upon Close Inspection
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..15,scores={wightRestless=..359999},sort=nearest] run kill @s
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..15,scores={wightRestless=..359999},sort=nearest] run kill @s
#Kills Day 12-14 Wights In The Daytime
execute as @e[type=stray,name=Wight] if entity @a[scores={wightAwake=1000..12577,wightRestless=288000..359999},sort=nearest] run kill @s
execute as @e[type=skeleton_horse,name="Wight Horse"] if entity @a[scores={wightAwake=1000..12566,wightRestless=288000..359999},sort=nearest] run kill @s
#Refresh Kill To Reset Wights To Day 15
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..50,scores={wightRestless=359999},sort=nearest] run kill @s
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=359999},sort=nearest] run kill @s
#Invisiblity For 12-14 Wights
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..50,scores={wightRestless=288000..359999},sort=nearest] run effect give @s minecraft:invisibility 1 255 true
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=288000..359999},sort=nearest] run effect give @s minecraft:invisibility 1 255 true
#Particle For Days 12-14
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=288000..359999},sort=nearest] run particle minecraft:cloud ~ ~1 ~ 0 0 0 0.025 10 normal
#Slowness At Daytime For Day 15 Wights 
execute as @e[type=skeleton_horse,name="Wight Horse"] if entity @a[scores={wightAwake=1000..12566,wightRestless=360000..},sort=nearest] run effect give @s minecraft:slowness 1 5 true
#Particle For Day 15 (A Lil' Bit Buggy) 
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=360000..},sort=nearest] run particle minecraft:cloud ~ ~0.5 ~ 0.2 0 0.2 0.035 7 normal
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=360000..},sort=nearest] run particle minecraft:smoke ^ ^1.45 ^1.2 0 0 0 0 5 normal
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..50,scores={wightRestless=360000..},sort=nearest] run particle minecraft:flame ^0.145 ^1.95 ^0.33 0 0 0 0 1 normal
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..50,scores={wightRestless=360000..},sort=nearest] run particle minecraft:flame ^-0.12 ^1.95 ^0.33 0 0 0 0 1 normal
#Resistance Thingy
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..50,scores={wightRestless=288000..},sort=nearest] run effect give @s minecraft:resistance 1 5 true
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=288000..},sort=nearest] run effect give @s minecraft:resistance 1 5 true
execute as @e[type=stray,name=Wight] at @s if entity @a[distance=..50,scores={wightRestless=288000..},sort=nearest] run effect give @s minecraft:fire_resistance 1 0 true
execute as @e[type=skeleton_horse,name="Wight Horse"] at @s if entity @a[distance=..50,scores={wightRestless=288000..},sort=nearest] run effect give @s minecraft:fire_resistance 1 0 true
#Gives Reward & Advancement/Resets Timer
execute as @a[scores={wightRestless=408000..}] run give @s minecraft:leather_chestplate{display:{Name:"{\"text\":\"Cloak Of The Wight\",\"color\":\"aqua\"}",Lore:["Gives Partial Invisibility When Worn"],color:4907519}} 1
execute as @a[scores={wightRestless=408000..}] run advancement grant @s only darkathame_comp1_13:shittymcadvancements/stalkingwight
scoreboard players set @a[scores={wightRestless=408000..}] wightRestless 0
#Cape Stuff
execute at @e[tag=!wightCstp,nbt={Inventory:[{Slot:102b,id:"minecraft:leather_chestplate",tag:{display:{Lore:["Gives Partial Invisibility When Worn"]}}}]}] run summon armor_stand ~ ~-2 ~ {NoGravity:1b,Marker:1b,Invisible:1b,ArmorItems:[{},{},{},{id:"minecraft:black_banner",Count:1b,tag:{BlockEntityTag:{Base:15,Patterns:[{Pattern:bo,Color:12},{Pattern:sku,Color:12},{Pattern:hhb,Color:12}]}}}],CustomName:"{\"text\":\"wightCape\"}"}
tag @a remove wightCstp
execute at @e[type=armor_stand,name=wightCape] as @a[limit=1,distance=..3,nbt={Inventory:[{Slot:102b,id:"minecraft:leather_chestplate",tag:{display:{Lore:["Gives Partial Invisibility When Worn"]}}}]}] run tag @s add wightCstp
execute as @a[tag=wightCstp] at @s run teleport @e[type=armor_stand,name=wightCape,limit=1,distance=..4] @s
execute at @a[tag=wightCstp] run teleport @e[type=armor_stand,name=wightCape,limit=1,distance=..4] ~ ~-2 ~
execute at @a[tag=!wightCstp] run kill @e[type=armor_stand,name=wightCape,distance=..4]
effect give @a[nbt={Inventory:[{Slot:102b,id:"minecraft:leather_chestplate",tag:{display:{Lore:["Gives Partial Invisibility When Worn"]}}}]}] minecraft:invisibility 1 0 true
#Reset
scoreboard players set @a wightAwake 0
