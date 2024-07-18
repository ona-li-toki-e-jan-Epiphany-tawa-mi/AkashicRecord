#Activates Kill Command Based On Local Damage
execute @a[score_Kikoku_min=1] ~ ~ ~ scoreboard players tag @e[r=10] add hurt {HurtTime:9s}
execute @a[score_Kikoku_min=1] ~ ~ ~ execute @e[tag=hurt,r=10] ~ ~ ~ say Was Consumed By The Kikoku
execute @a[score_Kikoku_min=1] ~ ~ ~ kill @e[tag=hurt,r=10]
