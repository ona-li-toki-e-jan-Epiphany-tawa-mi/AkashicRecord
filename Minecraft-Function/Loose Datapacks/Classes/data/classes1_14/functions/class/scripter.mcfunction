# Applies debuffs relevant to a weak body
execute as @e[team=scripter] run effect give @s minecraft:mining_fatigue 1 1 true
execute as @e[team=scripter] run effect give @s minecraft:weakness 1 255 true

# Makes it so the bow sucks for scripters
## Detects arrows shot by scripters
execute at @e[team=scripter] as @e[type=arrow,distance=..2,scores={arrTi=..1}] run tag @s add scri
## Cuts down the motion of arrows to emulate a weak pull
execute as @e[type=arrow,tag=scri] store result entity @s Motion[0] double 0.15 run data get entity @s Motion[0] 1
execute as @e[type=arrow,tag=scri] store result entity @s Motion[1] double 0.15 run data get entity @s Motion[1] 1
execute as @e[type=arrow,tag=scri] store result entity @s Motion[2] double 0.15 run data get entity @s Motion[2] 1

# I can't swim
## Makes it harder to swim up
execute as @e[team=scripter] at @s if block ~ ~-0.1 ~ minecraft:water run teleport @s ~ ~-0.1 ~
## Makes it harder to swim in general
execute as @e[team=scripter] at @s if block ~ ~-0.1 ~ minecraft:water run effect give @s minecraft:slowness 1 2 true

# Checks if a scripter is on a dropper before checking the items of it, for optimization
execute as @a[team=scripter] at @s if block ~ ~-1 ~ minecraft:dropper run tag @s add onDrp

# Checks for command block recipes in resource cost of 2^n, with n being 0 <= n <= 6
## Crafts one command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:1b},{Slot:1b,id:"minecraft:iron_ingot",Count:1b},{Slot:2b,id:"minecraft:polished_granite",Count:1b},{Slot:3b,id:"minecraft:iron_ingot",Count:1b},{Slot:4b,id:"minecraft:redstone",Count:1b},{Slot:5b,id:"minecraft:iron_ingot",Count:1b},{Slot:6b,id:"minecraft:polished_granite",Count:1b},{Slot:7b,id:"minecraft:iron_ingot",Count:1b},{Slot:8b,id:"minecraft:polished_granite",Count:1b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:1b}]}
## Crafts two command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:2b},{Slot:1b,id:"minecraft:iron_ingot",Count:2b},{Slot:2b,id:"minecraft:polished_granite",Count:2b},{Slot:3b,id:"minecraft:iron_ingot",Count:2b},{Slot:4b,id:"minecraft:redstone",Count:2b},{Slot:5b,id:"minecraft:iron_ingot",Count:2b},{Slot:6b,id:"minecraft:polished_granite",Count:2b},{Slot:7b,id:"minecraft:iron_ingot",Count:2b},{Slot:8b,id:"minecraft:polished_granite",Count:2b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:2b}]}
## Crafts four command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:4b},{Slot:1b,id:"minecraft:iron_ingot",Count:4b},{Slot:2b,id:"minecraft:polished_granite",Count:4b},{Slot:3b,id:"minecraft:iron_ingot",Count:4b},{Slot:4b,id:"minecraft:redstone",Count:4b},{Slot:5b,id:"minecraft:iron_ingot",Count:4b},{Slot:6b,id:"minecraft:polished_granite",Count:4b},{Slot:7b,id:"minecraft:iron_ingot",Count:4b},{Slot:8b,id:"minecraft:polished_granite",Count:4b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:4b}]}
## Crafts eight command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:8b},{Slot:1b,id:"minecraft:iron_ingot",Count:8b},{Slot:2b,id:"minecraft:polished_granite",Count:8b},{Slot:3b,id:"minecraft:iron_ingot",Count:8b},{Slot:4b,id:"minecraft:redstone",Count:8b},{Slot:5b,id:"minecraft:iron_ingot",Count:8b},{Slot:6b,id:"minecraft:polished_granite",Count:8b},{Slot:7b,id:"minecraft:iron_ingot",Count:8b},{Slot:8b,id:"minecraft:polished_granite",Count:8b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:8b}]}
## Crafts sixteen command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:16b},{Slot:1b,id:"minecraft:iron_ingot",Count:16b},{Slot:2b,id:"minecraft:polished_granite",Count:16b},{Slot:3b,id:"minecraft:iron_ingot",Count:16b},{Slot:4b,id:"minecraft:redstone",Count:16b},{Slot:5b,id:"minecraft:iron_ingot",Count:16b},{Slot:6b,id:"minecraft:polished_granite",Count:16b},{Slot:7b,id:"minecraft:iron_ingot",Count:16b},{Slot:8b,id:"minecraft:polished_granite",Count:16b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:16b}]}
## Crafts thirty two command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:32b},{Slot:1b,id:"minecraft:iron_ingot",Count:32b},{Slot:2b,id:"minecraft:polished_granite",Count:32b},{Slot:3b,id:"minecraft:iron_ingot",Count:32b},{Slot:4b,id:"minecraft:redstone",Count:32b},{Slot:5b,id:"minecraft:iron_ingot",Count:32b},{Slot:6b,id:"minecraft:polished_granite",Count:32b},{Slot:7b,id:"minecraft:iron_ingot",Count:32b},{Slot:8b,id:"minecraft:polished_granite",Count:32b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:32b}]}
## Crafts sixty four command block(s) at a time
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:0b,id:"minecraft:polished_granite",Count:64b},{Slot:1b,id:"minecraft:iron_ingot",Count:64b},{Slot:2b,id:"minecraft:polished_granite",Count:64b},{Slot:3b,id:"minecraft:iron_ingot",Count:64b},{Slot:4b,id:"minecraft:redstone",Count:64b},{Slot:5b,id:"minecraft:iron_ingot",Count:64b},{Slot:6b,id:"minecraft:polished_granite",Count:64b},{Slot:7b,id:"minecraft:iron_ingot",Count:64b},{Slot:8b,id:"minecraft:polished_granite",Count:64b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:command_block",Count:64b}]} 

# Allows crafting of debug stick
execute as @a[team=scripter,tag=onDrp] at @s if block ~ ~-1 ~ minecraft:dropper{Items:[{Slot:2b,id:"minecraft:command_block",Count:1b},{Slot:4b,id:"minecraft:stick",Count:1b},{Slot:6b,id:"minecraft:stick",Count:1b}]} run data merge block ~ ~-1 ~ {Items:[{Slot:4b,id:"minecraft:debug_stick",Count:1b,tag:{CustomModelData:1b,display:{Name:"{\"text\":\"Command Block Wand\",\"color\":\"green\"}"}}}]}

# Detects if a player is looking down onto a command block, for giving creative rights to manipulate command blocks without affecting the surrounding enviroment
## Tests for players facing directly down onto a command block
execute as @a[team=scripter,x_rotation=90] at @s if block ~ ~-1 ~ minecraft:command_block run tag @s add crtC
execute as @a[team=scripter,x_rotation=90] at @s if block ~ ~-1 ~ minecraft:repeating_command_block run tag @s add crtC
execute as @a[team=scripter,x_rotation=90] at @s if block ~ ~-1 ~ minecraft:chain_command_block run tag @s add crtC
## Quick disable so they have to be looking at the command block
execute as @a[team=scripter,x_rotation=..89] run tag @s remove crtC
## Sets gamemode to survival to prevent some abuse
execute as @a[team=scripter,tag=!crtC] run gamemode survival @s
## Sets gamemode to creative so a player can manipulate command blocks
execute as @a[team=scripter,tag=crtC] run gamemode creative @s

# Allows placement of command blocks
## Places down the command block, breaking any block in it's position
execute as @e[type=item,nbt={Item:{id:"minecraft:command_block",Count:1b},OnGround:1b}] at @s run setblock ~ ~ ~ minecraft:command_block destroy
## Kills command block item to complete placement process
execute as @e[type=item,nbt={Item:{id:"minecraft:command_block",Count:1b},OnGround:1b}] run kill @s

# Allows breaking of command blocks
## Tests for tnt items on command blocks and tags them
execute as @e[type=item,nbt={Item:{id:"minecraft:tnt",Count:1b},OnGround:1b}] at @s if block ~ ~-1 ~ minecraft:command_block run tag @s add dCmd 
execute as @e[type=item,nbt={Item:{id:"minecraft:tnt",Count:1b},OnGround:1b}] at @s if block ~ ~-1 ~ minecraft:repeating_command_block run tag @s add dCmd 
execute as @e[type=item,nbt={Item:{id:"minecraft:tnt",Count:1b},OnGround:1b}] at @s if block ~ ~-1 ~ minecraft:chain_command_block run tag @s add dCmd 
## Breaks command blocks that have tnt on them
execute as @e[type=item,tag=dCmd] at @s run setblock ~ ~-1 ~ minecraft:air replace
## Refunds the command block
execute as @e[type=item,tag=dCmd] at @s run give @p minecraft:command_block 1
## Refunds the tnt
execute as @e[type=item,tag=dCmd] at @s run give @p minecraft:tnt 1
## Plays a sound to emulate breaking the command block
execute as @e[type=item,tag=dCmd] at @s run playsound minecraft:block.stone.break block @a ~ ~ ~ 1 1 
## Kills the marked tnt item
execute as @e[type=item,tag=dCmd] run kill @s

# I happen to have a rather efficient metabolic process
## Actively stores the players food level
execute as @a[team=scripter] store result score @s arrTi run data get entity @s foodLevel 1
## Stores an instance of the players food level
execute as @a[team=scripter,tag=!dFc] store result score @s foLv run data get entity @s foodLevel 1
## Backup of default instance for independent hunger regeneration
execute as @a[team=scripter,tag=!dFc] run scoreboard players operation @s foLv2 = @s foLv 
## Resets function if the player regains hunger independently
execute as @a[team=scripter,tag=dFc] if score @s foLv2 < @s arrTi run tag @s remove dFc
## Subtracts the food level, acts as threshold for when food is refunded, higher thresholds make refund less frequent, please note that the two is a fake player
execute as @a[team=scripter,tag=!dFc] run scoreboard players operation @s foLv -= 2 const
## Tests if players current food level is less than the threshold, and tags
execute as @a[team=scripter,tag=dFc] if score @s foLv >= @s arrTi run tag @s add wFc
## Applies saturation, currently gives 0.5 shanks of refund for every shank lost
execute as @a[team=scripter,tag=wFc] run effect give @s minecraft:saturation 1 0 true
## Marks if the player was checked to control instance checking
execute as @a[team=scripter,scores={foLv=0..}] run tag @s add dFc
## Removes marker upon execution to reset 
execute as @a[team=scripter,tag=wFc] run tag @s remove dFc

# Reset
tag @a remove wFc
tag @e remove scri
tag @a remove onDrp
tag @a remove crtC