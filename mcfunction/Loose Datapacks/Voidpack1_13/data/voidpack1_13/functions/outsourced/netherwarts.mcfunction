#Detects Players Either Holding Or Standing In Nether Wart
execute as @a at @s if block ~ ~0.2 ~ minecraft:nether_wart run scoreboard players add @s wartexposureTime 1
scoreboard players add @a[nbt={SelectedItem:{id:"minecraft:nether_wart"}}] wartexposureTime 1
#Regulates Exposure
scoreboard players add @a[scores={wartexposureTime=1..},tag=!netherWarts] wartRexpoTime 1
scoreboard players remove @a[scores={wartRexpoTime=3..},tag=!netherWarts] wartexposureTime 1
scoreboard players set @a[scores={wartRexpoTime=3..}] wartRexpoTime 0
#Gives Infection
tag @a[scores={wartexposureTime=300..},tag=!imtWarts] add netherWarts
#Reset
scoreboard players set @a[tag=netherWarts] wartexposureTime 0
#Timer
scoreboard players add @a[tag=netherWarts] glowTimer 1
#Allows For Infection To Spread From One Player To Another
execute at @a[tag=netherWarts] as @a[tag=!netherwarts,distance=..2] run scoreboard players add @s wartexposureTime 1
#Grants Advancements
advancement grant @a[tag=netherWarts] only voidpack1_13:netherwartcontaigion
execute at @a[scores={glowTimer=..1},tag=netherWarts] run advancement grant @a[tag=netherWarts,distance=2,scores={glowTimer=2..}] only voidpack1_13:spreadnetherwarts
advancement grant @a[scores={glowTimer=744000..}] only voidpack1_13:survivenetherwarts
#Slowness Debuff For Infected Personal
effect give @a[scores={glowTimer=93000..185999}] minecraft:slowness 1 0 true
effect give @a[scores={glowTimer=186000..278999}] minecraft:slowness 1 1 true
effect give @a[scores={glowTimer=279000..371999}] minecraft:slowness 1 2 true
effect give @a[scores={glowTimer=372000..464999}] minecraft:slowness 1 3 true
effect give @a[scores={glowTimer=465000..557999}] minecraft:slowness 1 4 true
effect give @a[scores={glowTimer=558000..651000}] minecraft:slowness 1 5 true
effect give @a[scores={glowTimer=651001..669600}] minecraft:slowness 1 4 true
effect give @a[scores={glowTimer=669601..688200}] minecraft:slowness 1 3 true
effect give @a[scores={glowTimer=688201..706800}] minecraft:slowness 1 2 true
effect give @a[scores={glowTimer=706801..725400}] minecraft:slowness 1 1 true
effect give @a[scores={glowTimer=725401..744000}] minecraft:slowness 1 0 true
#Resistance Buff Infected Personal
effect give @a[scores={glowTimer=108500..216999}] minecraft:resistance 1 0 true
effect give @a[scores={glowTimer=217000..325499}] minecraft:resistance 1 1 true
effect give @a[scores={glowTimer=325500..433999}] minecraft:resistance 1 2 true
effect give @a[scores={glowTimer=434000..542499}] minecraft:resistance 1 3 true
effect give @a[scores={glowTimer=542500..651000}] minecraft:resistance 1 4 true
effect give @a[scores={glowTimer=651001..674250}] minecraft:resistance 1 3 true
effect give @a[scores={glowTimer=674251..697500}] minecraft:resistance 1 2 true
effect give @a[scores={glowTimer=697501..720750}] minecraft:resistance 1 1 true
effect give @a[scores={glowTimer=720751..744000}] minecraft:resistance 1 0 true
#Warts Peeling Off
give @a[scores={glowTimer=651000}] minecraft:nether_wart 1
give @a[scores={glowTimer=657200}] minecraft:nether_wart 1
give @a[scores={glowTimer=663400}] minecraft:nether_wart 1
give @a[scores={glowTimer=669600}] minecraft:nether_wart 1
give @a[scores={glowTimer=675800}] minecraft:nether_wart 1
give @a[scores={glowTimer=682000}] minecraft:nether_wart 2
give @a[scores={glowTimer=688200}] minecraft:nether_wart 2
give @a[scores={glowTimer=694400}] minecraft:nether_wart 2
give @a[scores={glowTimer=700600}] minecraft:nether_wart 2
give @a[scores={glowTimer=706800}] minecraft:nether_wart 3
give @a[scores={glowTimer=713000}] minecraft:nether_wart 3
give @a[scores={glowTimer=719200}] minecraft:nether_wart 3
give @a[scores={glowTimer=725400}] minecraft:nether_wart 4
give @a[scores={glowTimer=731600}] minecraft:nether_wart 4
give @a[scores={glowTimer=737800}] minecraft:nether_wart 5
give @a[scores={glowTimer=744000}] minecraft:nether_wart 6
#Nausea When Warts Peel Off
effect give @a[scores={glowTimer=651000}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=657200}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=663400}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=669600}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=675800}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=682000}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=688200}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=694400}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=700600}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=706800}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=713000}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=719200}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=725400}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=731600}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=737800}] minecraft:nausea 15 0 true
effect give @a[scores={glowTimer=744000}] minecraft:nausea 15 0 true
#Sound Of Warts Peeling Off
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=651000}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=657200}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=663400}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=669600}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=675800}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=682000}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=688200}] ~ ~ ~ 2 0.6
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=694400}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=700600}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=706800}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=713000}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=719200}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=725400}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=731600}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=737800}] ~ ~ ~ 2 0.6 
playsound minecraft:block.chorus_flower.grow master @a[scores={glowTimer=744000}] ~ ~ ~ 2 0.6 
#Stores Player Fire Values
execute as @a[scores={glowTimer=465000..651000}] store result score @s wartFireRest run data get entity @s Fire
#Fire Resistance
effect give @a[scores={wartFireRest=1..,wartFireCoold=2400..}] minecraft:fire_resistance 6 0
#Cooldown Of Fire Protection
scoreboard players set @a[scores={wartFireCoold=2400..,wartFireRest=1..}] wartFireCoold 0
scoreboard players add @a[scores={glowTimer=465000..651000}] wartFireCoold 1
#Detects Dead Infected
execute at @a[tag=netherWarts,scores={wartCloud=1..}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"infectionCloud\"}"}
#Stores The Infected Level Of The Dead
execute as @e[type=armor_stand,name=infectionCloud,tag=!stopwartCheck] store result score @s glowTimer run scoreboard players get @a[tag=netherWarts,limit=1,scores={wartCloud=1..},sort=nearest] glowTimer
#Tag To Stop Refresh Of The Store On The Armor Stand
tag @e[type=armor_stand,name=infectionCloud] add stopwartCheck
#A Range Particle Effect
execute at @e[type=armor_stand,name=infectionCloud] run particle dust 0.6 0.2 0.1 5 ~ ~ ~ 2 2 2 100 5 normal
#Infective Element
execute at @e[type=armor_stand,name=infectionCloud] as @a[tag=!netherWarts,distance=..5] run scoreboard players add @s wartexposureTime 10
#Loads Infection Level Onto Players
execute at @e[type=armor_stand,name=infectionCloud] as @a[tag=netherWarts,scores={glowTimer=..1},distance=..5] store result score @s[tag=!stopwartCheck] glowTimer run scoreboard players get @e[type=armor_stand,name=infectionCloud,limit=1,sort=nearest] glowTimer
#Tag To Stop Refresh Of The Store On The Armor Stand
execute at @e[type=armor_stand,name=infectionCloud] as @a[tag=netherWarts,distance=..5] run tag @s add stopwartCheck
#Timer
scoreboard players add @e[type=armor_stand,name=infectionCloud] wartRexpoTime 1
#Cleanup
kill @e[type=armor_stand,name=infectionCloud,scores={wartRexpoTime=6000..}]
#Wonderful Noises
execute at @e[type=armor_stand,name=infectionCloud] run playsound minecraft:block.chorus_flower.grow hostile @a ~ ~ ~ 2 0.5 1
#Reset
tag @a[scores={wartCloud=1..}] remove netherWarts
tag @a[tag=!netherWarts] remove stopwartCheck
scoreboard players set @a wartCloud 0
scoreboard players set @a[tag=!netherWarts] wartFireCoold 0
scoreboard players set @a wartFireRest 0
tag @a[scores={glowTimer=744000..}] add imtWarts 
tag @a[scores={glowTimer=744000..}] remove netherWarts 
scoreboard players set @a[tag=!netherWarts] glowTimer 0