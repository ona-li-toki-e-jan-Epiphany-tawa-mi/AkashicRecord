#The Summoning Process
#Detection Of Cow+Diamond, Initiates Summoning Of Diamond Man
execute at @e[type=minecraft:cow] at @e[type=minecraft:item,distance=..1,nbt={Item:{id:"minecraft:diamond"}}] run summon minecraft:armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Invisible:1b,CustomName:"{\"text\":\"diamondManS\"}"}
#Cleans Up Susan's Mess, She Is An Ugly Pig
execute at @e[type=armor_stand,name=diamondManS,tag=!dmGraphicMade] run kill @e[type=cow,distance=..1]
execute at @e[type=armor_stand,name=diamondManS,tag=!dmGraphicMade] run kill @e[type=item,distance=..1]
#Timer
scoreboard players add @e[type=armor_stand,name=diamondManS] dmansumtime 1
#Displays Some Graphics For The Summon
execute at @e[type=armor_stand,name=diamondManS,tag=!dmGraphicMade] run summon item ~ ~0.5 ~ {NoGravity:1b,PickupDelay:32767,Tags:["dmsumgraphic"],Item:{id:"minecraft:diamond",Count:1b}}
execute at @e[type=item,tag=dmsumgraphic] run tag @e[type=armor_stand,name=diamondManS,distance=..1] add dmGraphicMade
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=1..99}] run particle minecraft:portal ~ ~0.8 ~ 0 0 0 1 1 normal @a
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=100..199}] run particle minecraft:portal ~ ~0.8 ~ 0 0 0 1.2 2 normal @a
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=200..299}] run particle minecraft:portal ~ ~0.8 ~ 0 0 0 1.2 3 normal @a
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=300..399}] run particle minecraft:portal ~ ~0.8 ~ 0 0 0 1.3 4 normal @a
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=400..499}] run particle minecraft:portal ~ ~0.8 ~ 0 0 0 1.4 5 normal @a
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=500..}] run particle minecraft:portal ~ ~0.8 ~ 0 0 0 1.5 6 normal @a
#Summons The Almighty Diamond Man
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=600}] run summon minecraft:creeper ~ ~ ~ {NoGravity:1b,ExplosionRadius:6b,Fuse:0}
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=600}] run summon minecraft:zombie ~ ~-2 ~ {CustomNameVisible:1b,PersistenceRequired:1b,Health:1f,CustomName:"{\"text\":\"Diamond_Man\"}",HandItems:[{id:"minecraft:diamond",Count:6b},{}],HandDropChances:[1.0F,0.85F],ArmorItems:[{id:"minecraft:diamond_boots",Count:1b},{id:"minecraft:diamond_leggings",Count:1b},{id:"minecraft:diamond_chestplate",Count:1b},{id:"minecraft:diamond_block",Count:1b}],ArmorDropChances:[0.0F,0.0F,0.0F,0.0F],ActiveEffects:[{Id:11b,Amplifier:5b,Duration:5,ShowParticles:0b}],Attributes:[{Name:generic.maxHealth,Base:1},{Name:generic.followRange,Base:80},{Name:generic.knockbackResistance,Base:1.0},{Name:generic.attackDamage,Base:1000},{Name:generic.armor,Base:0}]}
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=600}] run say DIAMOND MAAAAAAAAAAAAAAANNNNNNN!!! (To The Tune Of [Pepsi Man Theme Song])
#Grants Advancement
execute at @e[type=zombie,name=Diamond_Man] as @a[distance=..10] run advancement grant @s only darkathame_comp1_13:shittymcadvancements/diamondman
#Wow Susan
execute at @e[type=armor_stand,name=diamondManS,scores={dmansumtime=601..}] run kill @e[type=item,tag=dmsumgraphic,distance=..1]
kill @e[type=armor_stand,name=diamondManS,scores={dmansumtime=601..}]
