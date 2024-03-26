#Somthing For Boris
scoreboard players add @e[type=zombie,name=Boris_Johnson] borety 1
#Works The Crossbreeding Cooldown, Willing Duration
scoreboard players add @e[type=!item,tag=dCbreed] breTcool 1
scoreboard players remove @e[type=!item,scores={cbredT=1..}] cbredT 1
#Some Stores
execute as @e[type=wolf] store result score @s cbredT run data get entity @s InLove 1 
execute as @e[type=cow] store result score @s cbredT run data get entity @s InLove 1
execute as @e[type=pig] store result score @s cbredT run data get entity @s InLove 1
#Tests For Correct Pumpkin+Carrot Conditions And Uses A Timer
execute as @e[type=item,nbt={Item:{id:"minecraft:carrot",Count:1b}}] at @s if block ~ ~-1 ~ minecraft:pumpkin run tag @s add pArrotC
scoreboard players add @e[type=item,tag=pArrotC] cbredT 1
#Particles Act As A Marker
execute at @e[type=item,tag=pArrotC,scores={cbredT=80..}] run particle minecraft:happy_villager ~ ~ ~ 0.25 0 0.25 2 1 normal
#Destroys Things/Summons Parrot/Plays Particles And Sound
execute at @e[type=item,tag=pArrotC,scores={cbredT=2400..}] run setblock ~ ~-1 ~ air replace
execute at @e[type=item,tag=pArrotC,scores={cbredT=2400..}] run summon minecraft:parrot ~ ~-1 ~
execute at @e[type=item,tag=pArrotC,scores={cbredT=2400..}] run particle minecraft:cloud ~ ~-1 ~ 0 0 0 0.2 200 normal
execute at @e[type=item,tag=pArrotC,scores={cbredT=2400..}] run playsound minecraft:entity.phantom.flap neutral @a ~ ~-1 ~ 1 1.3
kill @e[type=item,tag=pArrotC,scores={cbredT=2400..}]
#Removes Emerald Breeding Catalyst
execute at @e[type=villager,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:emerald"}}]
#Displays Those Heart Particles 
execute at @e[type=villager,scores={cbredT=599}] run particle minecraft:heart ~ ~0.5 ~ 0.5 0.5 0.5 2 8 normal
#Checks For Villagers Rdy To Get Down
execute at @e[type=item,nbt={Item:{id:"minecraft:emerald",Count:1b}}] as @e[type=villager,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
#Breeds Them
execute at @e[type=villager,scores={cbredT=1..},tag=!dCbreed] as @e[type=wolf,scores={cbredT=1..},distance=..1,tag=!dCbreed] run summon villager ~ ~ ~ {Health:1f,CareerLevel:1,CustomName:"{\"text\":\"Furliger\"}",ArmorItems:[{},{},{},{id:"minecraft:player_head",Count:1b,tag:{display:{Name:"{\"text\":\"Fur-suit Headpiece \"}"},SkullOwner:{Id:"69bb5136-8609-4ae0-838f-522426018964",Properties:{textures:[{Value:"eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvZWQxNzk3MDNjNjM2OTZiM2Q1YTcwMmE2NDNmNmRjOTYzMjc4NDFkZjQ1M2MyNTQ3OGVmMmFiY2IzZDMxNDBlYyJ9fX0="}]}}}}],ArmorDropChances:[0.85F,0.85F,0.85F,1.0F],Attributes:[{Name:generic.maxHealth,Base:1}],Offers:{Recipes:[{buy:{id:"minecraft:rotten_flesh",Count:7b},sell:{id:"minecraft:coarse_dirt",Count:2b}}]}}
#Stops Breeding
execute at @e[type=villager,scores={cbredT=1..},tag=!dCbreed] as @e[type=wolf,scores={cbredT=1..},distance=..1,tag=!dCbreed] run tag @e[distance=..1] add dCbreed
#Removes Catalyst Items
execute at @e[type=dolphin,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:cod"}}]
execute at @e[type=dolphin,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:salmon"}}]
execute at @e[type=dolphin,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:tropical_fish"}}]
execute at @e[type=dolphin,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:cooked_cod"}}]
execute at @e[type=dolphin,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:cooked_salmon"}}]
execute at @e[type=dolphin,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:pufferfish"}}]
#Displays Those Heart Particles 
execute at @e[type=dolphin,scores={cbredT=599}] run particle minecraft:heart ~ ~0.5 ~ 0.5 0.5 0.5 2 8 normal
#Allows For Breeding Dolphins
execute at @e[type=item,nbt={Item:{id:"minecraft:cod",Count:1b}}] as @e[type=dolphin,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:salmon",Count:1b}}] as @e[type=dolphin,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:tropical_fish",Count:1b}}] as @e[type=dolphin,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:cooked_cod",Count:1b}}] as @e[type=dolphin,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:cooked_salmon",Count:1b}}] as @e[type=dolphin,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:pufferfish",Count:1b}}] as @e[type=dolphin,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
#Breeds Them
execute at @e[type=dolphin,scores={cbredT=1..},tag=!dCbreed] as @e[type=cow,scores={cbredT=1..},distance=..1,tag=!dCbreed] run summon dolphin ~ ~ ~ {CustomNameVisible:0b,DeathLootTable:"null",PersistenceRequired:1b,CanFindTreasure:0b,Passengers:[{id:"minecraft:cow",DeathLootTable:"darkathame_comp1_13:sea_cow",PersistenceRequired:1b,CustomName:"{\"text\":\"Sea Cow\",\"color\":\"aqua\"}"}],CustomName:"{\"text\":\"cCowMut\"}"}
#Stops Breeding
execute at @e[type=dolphin,scores={cbredT=1..},tag=!dCbreed] as @e[type=cow,scores={cbredT=1..},distance=..1,tag=!dCbreed] run tag @e[distance=..1] add dCbreed
#Gives The Sea Cow And Its Mount Some Sick Dubs Broski
effect give @e[type=cow,name="Sea Cow"] minecraft:water_breathing 1 0 true
effect give @e[type=dolphin,name=cCowMut] minecraft:invisibility 1 0 true
#Force Pairs Entites
execute as @e[type=dolphin,name=cCowMut] at @s unless entity @e[type=cow,name="Sea Cow",distance=..2] run kill @s 
execute as @e[type=cow,name="Sea Cow"] at @s unless entity @e[type=dolphin,name=cCowMut,distance=..2] run kill @s 
#Removes Catalyst Items
execute at @e[type=polar_bear,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:cod"}}]
execute at @e[type=polar_bear,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:salmon"}}]
execute at @e[type=polar_bear,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:cooked_cod"}}]
execute at @e[type=polar_bear,scores={cbredT=599}] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:cooked_salmon"}}]
#Displays Those Heart Particles 
execute at @e[type=polar_bear,scores={cbredT=599}] run particle minecraft:heart ~ ~0.5 ~ 0.5 0.5 0.5 2 8 normal
#Allows For Breeding Dolphins
execute at @e[type=item,nbt={Item:{id:"minecraft:cod",Count:1b}}] as @e[type=polar_bear,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:salmon",Count:1b}}] as @e[type=polar_bear,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:cooked_cod",Count:1b}}] as @e[type=polar_bear,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
execute at @e[type=item,nbt={Item:{id:"minecraft:cooked_salmon",Count:1b}}] as @e[type=polar_bear,distance=..1,tag=!dCbreed] run scoreboard players set @s cbredT 600
#Breeds Them
execute at @e[type=polar_bear,scores={cbredT=1..},tag=!dCbreed] as @e[type=pig,scores={cbredT=1..},distance=..1,tag=!dCbreed] run summon zombie ~ ~ ~ {Silent:1b,CustomNameVisible:1b,DeathLootTable:"null",LeftHanded:1b,PersistenceRequired:1b,CanPickUpLoot:1b,Health:10f,CustomName:"{\"text\":\"Boris_Johnson\"}",ArmorItems:[{id:"minecraft:leather_boots",Count:1b,tag:{display:{color:0}}},{id:"minecraft:leather_leggings",Count:1b,tag:{display:{color:0}}},{id:"minecraft:leather_chestplate",Count:1b,tag:{display:{color:0}}},{id:"minecraft:player_head",Count:1b,tag:{SkullOwner:{Id:"4e9dd510-310a-4359-82ac-6e5b87a24df2",Properties:{textures:[{Value:"eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvMTExMjlmZTNjODkyYjRkMTEwMzhmNzNmYTdiZDZlMzU0NTY2YWFkYzJhYmVlNzE3ZDRjM2RlZTg2MTcxMzZkYyJ9fX0="}]}}}}],ArmorDropChances:[0.0F,0.0F,0.0F,0.0F],Attributes:[{Name:generic.maxHealth,Base:10},{Name:generic.followRange,Base:0}]}
#Oh
execute at @e[type=polar_bear,scores={cbredT=1..},tag=!dCbreed] as @e[type=pig,scores={cbredT=1..},distance=..1,tag=!dCbreed] run tellraw @a {"text":"Boris_Johnson joined the game.","color":"yellow"}
#Stops Breeding
execute at @e[type=polar_bear,scores={cbredT=1..},tag=!dCbreed] as @e[type=pig,scores={cbredT=1..},distance=..1,tag=!dCbreed] run tag @e[distance=..1] add dCbreed
#Randomizer
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["1boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["2boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["3boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["4boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["5boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["6boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["7boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["8boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["9boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["10boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["11boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["12boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["13boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["14boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,Tags:["15boris"],CustomName:"{\"text\":\"borSayc\"}"}
execute at @e[type=zombie,name=Boris_Johnson] run kill @e[type=armor_stand,name=borSayc,limit=14,sort=random,distance=..1] 
#Says Stuff
execute at @e[type=armor_stand,name=borSayc,tag=1boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> She's got dyed blonde hair and pouty lips, and a steely blue stare, like a sadistic nurse in a mental hospital.","color":"white"}
execute at @e[type=armor_stand,name=borSayc,tag=2boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> What a relief it must be for Blair to get out of England. It is said that the Queen has come to love the Commonwealth, partly because it supplies her with regular cheering crowds of flag-waving piccaninnies.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=3boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> Orientals... have larger brains and higher IQ scores. Blacks are at the other pole.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=4boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> Female students went to university because they have got to find men to marry.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=5boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> There was a young fellow from Ankara","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=5boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> Who was a terrific wankerer.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=5boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> Till he sowed his wild oats","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=5boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> With the help of a goat","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=5boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> But he didn’t even stop to thankera.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=6boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> Voting Tory will cause your wife to have bigger breasts and increase your chances of owning a BMW M3.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=7boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> You Pannini-Headed Mugwump","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=8boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> I don't see why people are so snooty about Channel 5. It has some respectable documentaries about the Second World War. It also devotes considerable airtime to investigations into lap-dancing, and other related and vital subjects","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=9boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> I would invite him to come and see the whole of London and take him round the city - except I wouldn’t want to expose any Londoners to any unnecessary risk of meeting Donald Trump.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=10boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> My speaking style was criticised by no less an authority than Arnold Schwarzenegger. It was a low moment, my friends, to have my rhetorical skills denounced by a monosyllabic Austrian cyborg.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=11boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> I think I was once given cocaine but I sneezed so it didn’t go up my nose. In fact, it may have been icing sugar.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=12boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> My policy on cake is pro having it and pro eating it.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=13boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> I don't believe that is necessarily any more dangerous than the many other risky things that people do with their free hands while driving - nose-picking, reading the paper, studying the A-Z, beating the children, and so on.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=14boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> It is just flipping unbelievable. He is a mixture of Harry Houdini and a greased piglet. He is barely human in his elusiveness. Nailing Blair is like trying to pin jelly to a wall.","color":"white"} 
execute at @e[type=armor_stand,name=borSayc,tag=15boris] as @e[type=zombie,name=Boris_Johnson,distance=..1] run tellraw @a {"text":"<Boris_Johnson> My chances of being PM are about as good as the chances of finding Elvis on Mars, or my being reincarnated as an olive.","color":"white"} 
#Reset
kill @e[type=armor_stand,name=borSayc] 
scoreboard players set @e[tag=dCbreed] cbredT 0
scoreboard players set @e[tag=!dCbreed] breTcool 0
tag @e remove pArrotC
tag @e[scores={breTcool=6000..}] remove dCbreed
scoreboard players set @e[type=zombie,name=Boris_Johnson,scores={borety=400..}] borety 0
