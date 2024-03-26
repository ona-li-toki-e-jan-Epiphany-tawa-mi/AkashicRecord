#Isolates Players And Deletes 1 Dirt 
execute at @a[scores={getdragonSword=1..}] at @e[type=item,distance=..6,nbt={Item:{id:"minecraft:dirt"}}] run summon item ~ ~ ~ {Age:-32768,Item:{id:"minecraft:diamond_sword",Count:1b,tag:{display:{Name:"{\"text\":\"Dragon Sword\"}"},Unbreakable:1b,AttributeModifiers:[{AttributeName:"generic.attackDamage",Name:"generic.attackDamage",Amount:2147483647,Operation:0,UUIDLeast:900277,UUIDMost:81894,Slot:"mainhand"},{AttributeName:"generic.attackSpeed",Name:"generic.attackSpeed",Amount:1024,Operation:0,UUIDLeast:436348,UUIDMost:561579,Slot:"mainhand"}]}}}
execute at @a[scores={getdragonSword=1..}] run kill @e[type=minecraft:item,distance=..6,limit=1,nbt={Item:{id:"minecraft:dirt"}}]
#Reset
scoreboard players set @a getdragonSword 0
#Detection Of A Held Dragon Sword
tag @a[nbt={SelectedItem:{id:"minecraft:diamond_sword",tag:{display:{Name:"{\"text\":\"Dragon Sword\"}"},Unbreakable:1b,AttributeModifiers:[{AttributeName:"generic.attackDamage",Name:"generic.attackDamage",Amount:2147483647,Operation:0,UUIDLeast:900277,UUIDMost:81894,Slot:"mainhand"},{AttributeName:"generic.attackSpeed",Name:"generic.attackSpeed",Amount:1024,Operation:0,UUIDLeast:436348,UUIDMost:561579,Slot:"mainhand"}]}}}] add holdingdragonSword
#Rips Apart Creative And Spectator Players
execute at @a[tag=holdingdragonSword] run kill @a[gamemode=creative,distance=1..10]
execute at @a[tag=holdingdragonSword] run kill @a[gamemode=spectator,distance=1..10]
#Grants Advancement
advancement grant @a[tag=holdingdragonSword] only darkathame_comp1_13:shittymcadvancements/dragonsword
#Another Reset
tag @a[tag=holdingdragonSword] remove holdingdragonSword
