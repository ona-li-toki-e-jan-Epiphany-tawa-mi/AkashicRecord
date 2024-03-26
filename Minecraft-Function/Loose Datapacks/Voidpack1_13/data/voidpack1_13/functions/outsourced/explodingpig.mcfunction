#Detects Pig+Gunpowder 
execute at @e[type=item,nbt={Item:{id:"minecraft:gunpowder"}}] as @e[type=pig,distance=..1,limit=1,tag=!explosivePig] run tag @s add explosivePig
#The Bit For Consuming The Gunpowder Once
execute as @e[type=pig,tag=explosivePig] at @s[tag=!stopKe] run kill @e[limit=1,distance=..1,type=item,nbt={Item:{id:"minecraft:gunpowder"}}]
tag @e[type=pig,tag=explosivePig] add stopKe
#Grants Advancement
execute at @e[type=pig,tag=explosivePig] as @a[distance=..6] run advancement grant @s only voidpack1_13:explosivepigs
#Detects Hurt Pigs
tag @e[type=pig,tag=explosivePig] add hurtE
tag @e[type=pig,nbt={HurtTime:0s},tag=explosivePig] remove hurtE
#A Small Particle Affect To Notify Players
execute at @e[type=pig,tag=explosivePig] run particle minecraft:smoke ~ ~0.6 ~ 0 0 0 0 1 normal @a
#The EXPPPLLOOOOOOOOOOSIONNNNNNNN!!
execute at @e[type=pig,tag=hurtE] run summon creeper ~ ~ ~ {ExplosionRadius:2b,Fuse:0,CustomName:"{\"text\":\"Pig\"}"}





