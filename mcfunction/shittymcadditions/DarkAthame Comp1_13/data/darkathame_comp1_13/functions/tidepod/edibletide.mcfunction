#Crafting Recipe For Tide Pods
execute at @e[type=item,nbt={Item:{id:"minecraft:ghast_tear",Count:1b}}] as @e[distance=..1,type=item,nbt={Item:{id:"minecraft:bucket",Count:1b}}] as @e[distance=..1,type=item,nbt={Item:{id:"minecraft:fermented_spider_eye",Count:1b}}] as @e[distance=..1,type=item,nbt={Item:{id:"minecraft:blaze_powder",Count:1b}}] as @e[distance=..1,type=item,nbt={Item:{id:"minecraft:snowball",Count:1b}}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"maketPod\"}"}
execute at @e[type=armor_stand,name=maketPod] run kill @e[type=item,nbt={Item:{id:"minecraft:ghast_tear"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=maketPod] run kill @e[type=item,nbt={Item:{id:"minecraft:bucket"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=maketPod] run kill @e[type=item,nbt={Item:{id:"minecraft:fermented_spider_eye"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=maketPod] run kill @e[type=item,nbt={Item:{id:"minecraft:blaze_powder"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=maketPod] run kill @e[type=item,nbt={Item:{id:"minecraft:snowball"}},distance=..1,limit=1]
execute at @e[type=armor_stand,name=maketPod] run summon item ~ ~ ~ {Item:{id:"minecraft:poisonous_potato",Count:1b,tag:{display:{Name:"{\"text\":\"Tide Pod\"}",Lore:["With Downy!","Sick Fuck"]}}}}
execute at @e[type=armor_stand,name=maketPod] run particle minecraft:firework ~ ~ ~ 0.1 0.1 0.1 0.05 100 normal
kill @e[type=armor_stand,name=maketPod]
#Tests If A Player Has Eaten A Tidepod
execute as @a[tag=!eatPod,nbt={SelectedItem:{id:"minecraft:poisonous_potato",tag:{display:{Name:"{\"text\":\"Tide Pod\"}",Lore:["With Downy!","Sick Fuck"]}}}}] run tag @s add eatPod
#Applies Ill Effects
execute as @a[scores={tideEat=1..},tag=eatPod] run tellraw @s {"text":"Your Lungs Fill With Fluid!","color":"red","bold":true,"italic":true,"hoverEvent":{"action":"show_text","value":"It's What You Get For Eating Tide Pods"}}
execute as @a[scores={tideEat=1..},tag=eatPod] run effect give @s minecraft:wither 10000 1 true
execute as @a[scores={tideEat=1..},tag=eatPod] run effect give @s minecraft:poison 10000 1 true
#Gives Advancement
advancement grant @a[scores={tideEat=1..},tag=eatPod] only darkathame_comp1_13:shittymcadvancements/tidepod
#Reset
scoreboard players add @a[tag=eatPod] tideEx 1
scoreboard players set @a tideEat 0
tag @a[scores={tideEx=2..}] remove eatPod
scoreboard players set @a[scores={tideEx=2..}] tideEx 0
