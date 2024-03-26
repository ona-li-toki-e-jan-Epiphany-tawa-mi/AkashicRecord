#Initiates Kikoku Functions, Gives Kikoku
scoreboard objectives add Kikoku dummy Kikoku
setblock ~ ~-1.0 ~ minecraft:repeating_command_block 0 replace {Command:"/function funstuff:kikoku/kikoku",auto:1b}
give DarkLordDudeALT minecraft:iron_sword 1 0 {display:{Name:"Kikoku",Lore:["It Tears At You"]}}
