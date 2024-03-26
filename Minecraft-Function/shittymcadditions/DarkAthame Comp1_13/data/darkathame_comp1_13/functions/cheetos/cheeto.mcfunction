#Crafting Recipe For The Cheeto
execute at @e[limit=1,type=item,nbt={Item:{id:"minecraft:wheat",Count:1b}}] as @e[distance=..1,type=item,nbt={Item:{id:"minecraft:milk_bucket"}}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"makeCheeto\"}"}
execute at @e[type=armor_stand,name=makeCheeto] run kill @e[type=item,nbt={Item:{id:"minecraft:wheat"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=makeCheeto] run kill @e[type=item,nbt={Item:{id:"minecraft:milk_bucket"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=makeCheeto] run summon item ~ ~ ~ {Item:{id:"minecraft:dried_kelp",Count:1b,tag:{display:{Name:"{\"text\":\"Cheeto\"}",Lore:["Pretty Sticky"]},HideFlags:1,Enchantments:[{id:"minecraft:fire_aspect",lvl:1}]}}}
execute at @e[type=armor_stand,name=makeCheeto] run summon item ~ ~ ~ {Item:{id:"minecraft:bucket",Count:1b}}
execute at @e[type=armor_stand,name=makeCheeto] run particle minecraft:firework ~ ~ ~ 0.1 0.1 0.1 0.05 100 normal
kill @e[type=armor_stand,name=makeCheeto]
#An Oddly Complex System To Test If A Player Has Eaten A Cheeto
execute as @a[tag=!eatCheeto,nbt={SelectedItem:{id:"minecraft:dried_kelp",tag:{display:{Name:"{\"text\":\"Cheeto\"}",Lore:["Pretty Sticky"]},HideFlags:1}}}] run tag @s add eatCheeto
#Randomizer
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"chetCstic\"}"}
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"chetCstic\"}"}
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["succsD"],CustomName:"{\"text\":\"chetCstic\"}"}
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"chetCstic\"}"}
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"chetCstic\"}"}
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"chetCstic\"}"}
execute at @a[scores={eatCheeto=1..},tag=eatCheeto] run kill @e[type=armor_stand,name=chetCstic,limit=4,sort=random,distance=..1]
#Applies Slowness
execute at @e[type=armor_stand,name=chetCstic,tag=succsD] as @a[distance=..1,limit=1] run effect give @s minecraft:slowness 20 6 true
execute at @e[type=armor_stand,name=chetCstic,tag=succsD] as @a[distance=..1,limit=1] run effect give @s minecraft:jump_boost 20 128 true
execute at @e[type=armor_stand,name=chetCstic,tag=succsD] as @a[distance=..1,limit=1] run tellraw @s {"text":"Your Super Sticky!","color":"green","bold":true,"italic":true,"hoverEvent":{"action":"show_text","value":"Ewww"}}
#Gives Advancement
execute at @e[type=armor_stand,name=chetCstic,tag=succsD] as @a[distance=..1,limit=1] run advancement grant @s only darkathame_comp1_13:shittymcadvancements/cheetos
#Reset
kill @e[type=armor_stand,name=chetCstic]
#An Oddly Complex System To Test If A Player Has Eaten A Cheeto
scoreboard players add @a[tag=eatCheeto] cheetoEx 1
scoreboard players set @a eatCheeto 0
tag @a[scores={cheetoEx=2..}] remove eatCheeto
scoreboard players set @a[scores={cheetoEx=2..}] cheetoEx 0
