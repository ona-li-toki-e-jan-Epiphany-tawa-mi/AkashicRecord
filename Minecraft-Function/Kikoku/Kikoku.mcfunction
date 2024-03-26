#Resets Scoreboards
scoreboard players tag @e[tag=hurt] remove hurt
scoreboard players set @a Kikoku 0

#Detects Kikoku, Runs Kill Function
scoreboard players set @a Kikoku 1 {SelectedItem:{id:"minecraft:iron_sword",tag:{display:{Name:"Kikoku",Lore:["It Tears At You"]}}}}
function funstuff:kikoku/kikokuhit if @a[score_Kikoku_min=1]
