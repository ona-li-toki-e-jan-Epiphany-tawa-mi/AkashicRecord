#Adds To phanGrowth
execute as @a[scores={phanRestless=24001..}] run scoreboard players add @s phanGrowth 1
#Resets
execute as @a[scores={phanRestless=24001..}] run scoreboard players set @s phanRestless 1
execute as @a[scores={phanRestless=0}] run scoreboard players set @s phanGrowth 0
#Increases Size Of Phantoms
execute at @a as @e[type=phantom,distance=..50] store result entity @s Size float 1 run scoreboard players get @p phanGrowth
