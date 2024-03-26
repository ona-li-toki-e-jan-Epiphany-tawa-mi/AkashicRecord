#Spawns possible messages at player position.
execute at @a[scores={gamerKills=1..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Tags:["ebic"],CustomName:"{\"text\":\"ranKill\"}"}
execute at @a[scores={gamerKills=1..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Tags:["bag"],CustomName:"{\"text\":\"ranKill\"}"}
execute at @a[scores={gamerKills=1..}] run summon armor_stand ~ ~ ~ {CustomNameVisible:0b,NoGravity:1b,Marker:1b,Tags:["royal"],CustomName:"{\"text\":\"ranKill\"}"}
#Culls the spawn
execute at @a[scores={gamerKills=1..}] run kill @e[type=armor_stand,name=ranKill,limit=2,sort=random,distance=..1] 
#Screams fresh from hell
execute at @a[scores={gamerKills=1..}] as @e[type=armor_stand,name=ranKill,tag=ebic,distance=..1] run tellraw @a [{"selector":"@a[scores={gamerKills=1..}]","color":"red","bold":"true"},{"text":" got an","color":"yellow","underlined":"true","strikethrough":"true"},{"text":" EPIC kill!1!!111!","color":"red","italic":"true"}]
execute at @a[scores={gamerKills=1..}] as @e[type=armor_stand,name=ranKill,tag=bag,distance=..1] run tellraw @a [{"selector":"@a[scores={gamerKills=1..}]","color":"red","bold":"true"},{"text":" secured the","color":"yellow","underlined":"true","strikethrough":"true"},{"text":" Bag!","bold":"true","italic":"true","hoverEvent":{"action":"show_text","value":"bag? more like despacitwo!"}}]
execute at @a[scores={gamerKills=1..}] as @e[type=armor_stand,name=ranKill,tag=royal,distance=..1] run tellraw @a [{"selector":"@a[scores={gamerKills=1..}]","color":"red","bold":"true"},{"text":" won the","color":"yellow","underlined":"true","strikethrough":"true"},{"text":" EPIC Viktory Royal!!!11!","bold":"true","italic":"true","hoverEvent":{"action":"show_text","value":"bag? more like despacitwo!"}}]
#Reset
kill @e[name=ranKill]
scoreboard players set @a gamerKills 0
