# Gives speed for grass pathing
execute as @e at @s if block ~ ~-0.2 ~ minecraft:grass_path run effect give @s minecraft:speed 1 0 true
## Timer for arrow life
scoreboard players add @e[type=arrow] arrTi 1