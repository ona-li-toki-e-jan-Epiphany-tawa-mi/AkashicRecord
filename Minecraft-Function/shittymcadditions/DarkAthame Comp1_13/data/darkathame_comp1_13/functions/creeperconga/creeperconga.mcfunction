#Marks the front of the conga line
execute unless entity @e[type=creeper,scores={congaPos=1}] at @r run scoreboard players set @e[type=creeper,limit=1,sort=nearest] congaPos 1
#Sets position/moves conga line parts/corrects line position
execute at @e[type=creeper,scores={congaPos=1}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=2}] run scoreboard players set @s congaPos 2
execute as @e[type=creeper,scores={congaPos=2}] at @s if entity @e[type=creeper,scores={congaPos=1},distance=1..20] facing entity @e[type=creeper,scores={congaPos=1}] feet run teleport ^ ^ ^0.15
execute at @e[type=creeper,scores={congaPos=2}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=3}] run scoreboard players set @s congaPos 3
execute as @e[type=creeper,scores={congaPos=3}] at @s if entity @e[type=creeper,scores={congaPos=2},distance=1..20] facing entity @e[type=creeper,scores={congaPos=2}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=3}] at @s unless entity @e[type=creeper,scores={congaPos=2},distance=..10] run scoreboard players set @s congaPos 2
execute at @e[type=creeper,scores={congaPos=3}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=4}] run scoreboard players set @s congaPos 4
execute as @e[type=creeper,scores={congaPos=4}] at @s if entity @e[type=creeper,scores={congaPos=3},distance=1..20] facing entity @e[type=creeper,scores={congaPos=3}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=4}] at @s unless entity @e[type=creeper,scores={congaPos=3},distance=..10] run scoreboard players set @s congaPos 3
execute at @e[type=creeper,scores={congaPos=4}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=5}] run scoreboard players set @s congaPos 5
execute as @e[type=creeper,scores={congaPos=5}] at @s if entity @e[type=creeper,scores={congaPos=4},distance=1..20] facing entity @e[type=creeper,scores={congaPos=4}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=5}] at @s unless entity @e[type=creeper,scores={congaPos=4},distance=..10] run scoreboard players set @s congaPos 4
execute at @e[type=creeper,scores={congaPos=5}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=6}] run scoreboard players set @s congaPos 6
execute as @e[type=creeper,scores={congaPos=6}] at @s if entity @e[type=creeper,scores={congaPos=5},distance=1..20] facing entity @e[type=creeper,scores={congaPos=5}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=6}] at @s unless entity @e[type=creeper,scores={congaPos=5},distance=..10] run scoreboard players set @s congaPos 5
execute at @e[type=creeper,scores={congaPos=6}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=7}] run scoreboard players set @s congaPos 7
execute as @e[type=creeper,scores={congaPos=7}] at @s if entity @e[type=creeper,scores={congaPos=6},distance=1..20] facing entity @e[type=creeper,scores={congaPos=6}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=7}] at @s unless entity @e[type=creeper,scores={congaPos=6},distance=..10] run scoreboard players set @s congaPos 6
execute at @e[type=creeper,scores={congaPos=7}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=8}] run scoreboard players set @s congaPos 8
execute as @e[type=creeper,scores={congaPos=8}] at @s if entity @e[type=creeper,scores={congaPos=7},distance=1..20] facing entity @e[type=creeper,scores={congaPos=7}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=8}] at @s unless entity @e[type=creeper,scores={congaPos=7},distance=..10] run scoreboard players set @s congaPos 7
execute at @e[type=creeper,scores={congaPos=8}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=9}] run scoreboard players set @s congaPos 9
execute as @e[type=creeper,scores={congaPos=9}] at @s if entity @e[type=creeper,scores={congaPos=8},distance=1..20] facing entity @e[type=creeper,scores={congaPos=8}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=9}] at @s unless entity @e[type=creeper,scores={congaPos=8},distance=..10] run scoreboard players set @s congaPos 8
execute at @e[type=creeper,scores={congaPos=9}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=10}] run scoreboard players set @s congaPos 10
execute as @e[type=creeper,scores={congaPos=10}] at @s if entity @e[type=creeper,scores={congaPos=9},distance=1..20] facing entity @e[type=creeper,scores={congaPos=9}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=10}] at @s unless entity @e[type=creeper,scores={congaPos=9},distance=..10] run scoreboard players set @s congaPos 9
execute at @e[type=creeper,scores={congaPos=10}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=11}] run scoreboard players set @s congaPos 11
execute as @e[type=creeper,scores={congaPos=11}] at @s if entity @e[type=creeper,scores={congaPos=10},distance=1..20] facing entity @e[type=creeper,scores={congaPos=10}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=11}] at @s unless entity @e[type=creeper,scores={congaPos=10},distance=..10] run scoreboard players set @s congaPos 10
execute at @e[type=creeper,scores={congaPos=11}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=12}] run scoreboard players set @s congaPos 12
execute as @e[type=creeper,scores={congaPos=12}] at @s if entity @e[type=creeper,scores={congaPos=11},distance=1..20] facing entity @e[type=creeper,scores={congaPos=11}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=12}] at @s unless entity @e[type=creeper,scores={congaPos=11},distance=..10] run scoreboard players set @s congaPos 11
execute at @e[type=creeper,scores={congaPos=12}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=13}] run scoreboard players set @s congaPos 13
execute as @e[type=creeper,scores={congaPos=13}] at @s if entity @e[type=creeper,scores={congaPos=12},distance=1..20] facing entity @e[type=creeper,scores={congaPos=12}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=13}] at @s unless entity @e[type=creeper,scores={congaPos=12},distance=..10] run scoreboard players set @s congaPos 12
execute at @e[type=creeper,scores={congaPos=13}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=14}] run scoreboard players set @s congaPos 14
execute as @e[type=creeper,scores={congaPos=14}] at @s if entity @e[type=creeper,scores={congaPos=13},distance=1..20] facing entity @e[type=creeper,scores={congaPos=13}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=14}] at @s unless entity @e[type=creeper,scores={congaPos=13},distance=..10] run scoreboard players set @s congaPos 13
execute at @e[type=creeper,scores={congaPos=14}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=15}] run scoreboard players set @s congaPos 15
execute as @e[type=creeper,scores={congaPos=15}] at @s if entity @e[type=creeper,scores={congaPos=14},distance=1..20] facing entity @e[type=creeper,scores={congaPos=14}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=15}] at @s unless entity @e[type=creeper,scores={congaPos=14},distance=..10] run scoreboard players set @s congaPos 14
execute at @e[type=creeper,scores={congaPos=15}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=16}] run scoreboard players set @s congaPos 16
execute as @e[type=creeper,scores={congaPos=16}] at @s if entity @e[type=creeper,scores={congaPos=15},distance=1..20] facing entity @e[type=creeper,scores={congaPos=15}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=16}] at @s unless entity @e[type=creeper,scores={congaPos=15},distance=..10] run scoreboard players set @s congaPos 15
execute at @e[type=creeper,scores={congaPos=16}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=17}] run scoreboard players set @s congaPos 17
execute as @e[type=creeper,scores={congaPos=17}] at @s if entity @e[type=creeper,scores={congaPos=16},distance=1..20] facing entity @e[type=creeper,scores={congaPos=16}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=17}] at @s unless entity @e[type=creeper,scores={congaPos=16},distance=..10] run scoreboard players set @s congaPos 16
execute at @e[type=creeper,scores={congaPos=17}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=18}] run scoreboard players set @s congaPos 18
execute as @e[type=creeper,scores={congaPos=18}] at @s if entity @e[type=creeper,scores={congaPos=17},distance=1..20] facing entity @e[type=creeper,scores={congaPos=17}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=18}] at @s unless entity @e[type=creeper,scores={congaPos=17},distance=..10] run scoreboard players set @s congaPos 17
execute at @e[type=creeper,scores={congaPos=18}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=19}] run scoreboard players set @s congaPos 19
execute as @e[type=creeper,scores={congaPos=19}] at @s if entity @e[type=creeper,scores={congaPos=18},distance=1..20] facing entity @e[type=creeper,scores={congaPos=18}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=19}] at @s unless entity @e[type=creeper,scores={congaPos=18},distance=..10] run scoreboard players set @s congaPos 18
execute at @e[type=creeper,scores={congaPos=19}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=20}] run scoreboard players set @s congaPos 20
execute as @e[type=creeper,scores={congaPos=20}] at @s if entity @e[type=creeper,scores={congaPos=19},distance=1..20] facing entity @e[type=creeper,scores={congaPos=19}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=20}] at @s unless entity @e[type=creeper,scores={congaPos=19},distance=..10] run scoreboard players set @s congaPos 19
execute at @e[type=creeper,scores={congaPos=20}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=21}] run scoreboard players set @s congaPos 21
execute as @e[type=creeper,scores={congaPos=21}] at @s if entity @e[type=creeper,scores={congaPos=20},distance=1..20] facing entity @e[type=creeper,scores={congaPos=20}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=21}] at @s unless entity @e[type=creeper,scores={congaPos=20},distance=..10] run scoreboard players set @s congaPos 20
execute at @e[type=creeper,scores={congaPos=21}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=22}] run scoreboard players set @s congaPos 22
execute as @e[type=creeper,scores={congaPos=22}] at @s if entity @e[type=creeper,scores={congaPos=21},distance=1..20] facing entity @e[type=creeper,scores={congaPos=21}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=22}] at @s unless entity @e[type=creeper,scores={congaPos=21},distance=..10] run scoreboard players set @s congaPos 21
execute at @e[type=creeper,scores={congaPos=22}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=23}] run scoreboard players set @s congaPos 23
execute as @e[type=creeper,scores={congaPos=23}] at @s if entity @e[type=creeper,scores={congaPos=22},distance=1..20] facing entity @e[type=creeper,scores={congaPos=22}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=23}] at @s unless entity @e[type=creeper,scores={congaPos=22},distance=..10] run scoreboard players set @s congaPos 22
execute at @e[type=creeper,scores={congaPos=23}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=24}] run scoreboard players set @s congaPos 24
execute as @e[type=creeper,scores={congaPos=24}] at @s if entity @e[type=creeper,scores={congaPos=23},distance=1..20] facing entity @e[type=creeper,scores={congaPos=23}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=24}] at @s unless entity @e[type=creeper,scores={congaPos=23},distance=..10] run scoreboard players set @s congaPos 23
execute at @e[type=creeper,scores={congaPos=24}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=25}] run scoreboard players set @s congaPos 25
execute as @e[type=creeper,scores={congaPos=25}] at @s if entity @e[type=creeper,scores={congaPos=24},distance=1..20] facing entity @e[type=creeper,scores={congaPos=24}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=25}] at @s unless entity @e[type=creeper,scores={congaPos=24},distance=..10] run scoreboard players set @s congaPos 24
execute at @e[type=creeper,scores={congaPos=25}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=26}] run scoreboard players set @s congaPos 26
execute as @e[type=creeper,scores={congaPos=26}] at @s if entity @e[type=creeper,scores={congaPos=25},distance=1..20] facing entity @e[type=creeper,scores={congaPos=25}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=26}] at @s unless entity @e[type=creeper,scores={congaPos=25},distance=..10] run scoreboard players set @s congaPos 25
execute at @e[type=creeper,scores={congaPos=26}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=27}] run scoreboard players set @s congaPos 27
execute as @e[type=creeper,scores={congaPos=27}] at @s if entity @e[type=creeper,scores={congaPos=26},distance=1..20] facing entity @e[type=creeper,scores={congaPos=26}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=27}] at @s unless entity @e[type=creeper,scores={congaPos=26},distance=..10] run scoreboard players set @s congaPos 26
execute at @e[type=creeper,scores={congaPos=27}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=28}] run scoreboard players set @s congaPos 28
execute as @e[type=creeper,scores={congaPos=28}] at @s if entity @e[type=creeper,scores={congaPos=27},distance=1..20] facing entity @e[type=creeper,scores={congaPos=27}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=28}] at @s unless entity @e[type=creeper,scores={congaPos=27},distance=..10] run scoreboard players set @s congaPos 27
execute at @e[type=creeper,scores={congaPos=28}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=29}] run scoreboard players set @s congaPos 29
execute as @e[type=creeper,scores={congaPos=29}] at @s if entity @e[type=creeper,scores={congaPos=28},distance=1..20] facing entity @e[type=creeper,scores={congaPos=28}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=29}] at @s unless entity @e[type=creeper,scores={congaPos=28},distance=..10] run scoreboard players set @s congaPos 28
execute at @e[type=creeper,scores={congaPos=29}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=30}] run scoreboard players set @s congaPos 30
execute as @e[type=creeper,scores={congaPos=30}] at @s if entity @e[type=creeper,scores={congaPos=29},distance=1..20] facing entity @e[type=creeper,scores={congaPos=29}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=30}] at @s unless entity @e[type=creeper,scores={congaPos=29},distance=..10] run scoreboard players set @s congaPos 29
execute at @e[type=creeper,scores={congaPos=30}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=31}] run scoreboard players set @s congaPos 31
execute as @e[type=creeper,scores={congaPos=31}] at @s if entity @e[type=creeper,scores={congaPos=30},distance=1..20] facing entity @e[type=creeper,scores={congaPos=30}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=31}] at @s unless entity @e[type=creeper,scores={congaPos=30},distance=..10] run scoreboard players set @s congaPos 30
execute at @e[type=creeper,scores={congaPos=31}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=32}] run scoreboard players set @s congaPos 32
execute as @e[type=creeper,scores={congaPos=32}] at @s if entity @e[type=creeper,scores={congaPos=31},distance=1..20] facing entity @e[type=creeper,scores={congaPos=31}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=32}] at @s unless entity @e[type=creeper,scores={congaPos=31},distance=..10] run scoreboard players set @s congaPos 31
execute at @e[type=creeper,scores={congaPos=32}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=33}] run scoreboard players set @s congaPos 33
execute as @e[type=creeper,scores={congaPos=33}] at @s if entity @e[type=creeper,scores={congaPos=32},distance=1..20] facing entity @e[type=creeper,scores={congaPos=32}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=33}] at @s unless entity @e[type=creeper,scores={congaPos=32},distance=..10] run scoreboard players set @s congaPos 32
execute at @e[type=creeper,scores={congaPos=33}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=34}] run scoreboard players set @s congaPos 34
execute as @e[type=creeper,scores={congaPos=34}] at @s if entity @e[type=creeper,scores={congaPos=33},distance=1..20] facing entity @e[type=creeper,scores={congaPos=33}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=34}] at @s unless entity @e[type=creeper,scores={congaPos=33},distance=..10] run scoreboard players set @s congaPos 33
execute at @e[type=creeper,scores={congaPos=34}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=35}] run scoreboard players set @s congaPos 35
execute as @e[type=creeper,scores={congaPos=35}] at @s if entity @e[type=creeper,scores={congaPos=34},distance=1..20] facing entity @e[type=creeper,scores={congaPos=34}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=35}] at @s unless entity @e[type=creeper,scores={congaPos=34},distance=..10] run scoreboard players set @s congaPos 34
execute at @e[type=creeper,scores={congaPos=35}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=36}] run scoreboard players set @s congaPos 36
execute as @e[type=creeper,scores={congaPos=36}] at @s if entity @e[type=creeper,scores={congaPos=35},distance=1..20] facing entity @e[type=creeper,scores={congaPos=35}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=36}] at @s unless entity @e[type=creeper,scores={congaPos=35},distance=..10] run scoreboard players set @s congaPos 35
execute at @e[type=creeper,scores={congaPos=36}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=37}] run scoreboard players set @s congaPos 37
execute as @e[type=creeper,scores={congaPos=37}] at @s if entity @e[type=creeper,scores={congaPos=36},distance=1..20] facing entity @e[type=creeper,scores={congaPos=36}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=37}] at @s unless entity @e[type=creeper,scores={congaPos=36},distance=..10] run scoreboard players set @s congaPos 36
execute at @e[type=creeper,scores={congaPos=37}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=38}] run scoreboard players set @s congaPos 38
execute as @e[type=creeper,scores={congaPos=38}] at @s if entity @e[type=creeper,scores={congaPos=37},distance=1..20] facing entity @e[type=creeper,scores={congaPos=37}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=38}] at @s unless entity @e[type=creeper,scores={congaPos=37},distance=..10] run scoreboard players set @s congaPos 37
execute at @e[type=creeper,scores={congaPos=38}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=39}] run scoreboard players set @s congaPos 39
execute as @e[type=creeper,scores={congaPos=39}] at @s if entity @e[type=creeper,scores={congaPos=38},distance=1..20] facing entity @e[type=creeper,scores={congaPos=38}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=39}] at @s unless entity @e[type=creeper,scores={congaPos=38},distance=..10] run scoreboard players set @s congaPos 38
execute at @e[type=creeper,scores={congaPos=39}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=40}] run scoreboard players set @s congaPos 40
execute as @e[type=creeper,scores={congaPos=40}] at @s if entity @e[type=creeper,scores={congaPos=39},distance=1..20] facing entity @e[type=creeper,scores={congaPos=39}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=40}] at @s unless entity @e[type=creeper,scores={congaPos=39},distance=..10] run scoreboard players set @s congaPos 39
execute at @e[type=creeper,scores={congaPos=40}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=41}] run scoreboard players set @s congaPos 41
execute as @e[type=creeper,scores={congaPos=41}] at @s if entity @e[type=creeper,scores={congaPos=40},distance=1..20] facing entity @e[type=creeper,scores={congaPos=40}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=41}] at @s unless entity @e[type=creeper,scores={congaPos=40},distance=..10] run scoreboard players set @s congaPos 40
execute at @e[type=creeper,scores={congaPos=41}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=42}] run scoreboard players set @s congaPos 42
execute as @e[type=creeper,scores={congaPos=42}] at @s if entity @e[type=creeper,scores={congaPos=41},distance=1..20] facing entity @e[type=creeper,scores={congaPos=41}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=42}] at @s unless entity @e[type=creeper,scores={congaPos=41},distance=..10] run scoreboard players set @s congaPos 41
execute at @e[type=creeper,scores={congaPos=42}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=43}] run scoreboard players set @s congaPos 43
execute as @e[type=creeper,scores={congaPos=43}] at @s if entity @e[type=creeper,scores={congaPos=42},distance=1..20] facing entity @e[type=creeper,scores={congaPos=42}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=43}] at @s unless entity @e[type=creeper,scores={congaPos=42},distance=..10] run scoreboard players set @s congaPos 42
execute at @e[type=creeper,scores={congaPos=43}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=44}] run scoreboard players set @s congaPos 44
execute as @e[type=creeper,scores={congaPos=44}] at @s if entity @e[type=creeper,scores={congaPos=43},distance=1..20] facing entity @e[type=creeper,scores={congaPos=43}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=44}] at @s unless entity @e[type=creeper,scores={congaPos=43},distance=..10] run scoreboard players set @s congaPos 43
execute at @e[type=creeper,scores={congaPos=44}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=45}] run scoreboard players set @s congaPos 45
execute as @e[type=creeper,scores={congaPos=45}] at @s if entity @e[type=creeper,scores={congaPos=44},distance=1..20] facing entity @e[type=creeper,scores={congaPos=44}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=45}] at @s unless entity @e[type=creeper,scores={congaPos=44},distance=..10] run scoreboard players set @s congaPos 44
execute at @e[type=creeper,scores={congaPos=45}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=46}] run scoreboard players set @s congaPos 46
execute as @e[type=creeper,scores={congaPos=46}] at @s if entity @e[type=creeper,scores={congaPos=45},distance=1..20] facing entity @e[type=creeper,scores={congaPos=45}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=46}] at @s unless entity @e[type=creeper,scores={congaPos=45},distance=..10] run scoreboard players set @s congaPos 45
execute at @e[type=creeper,scores={congaPos=46}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=47}] run scoreboard players set @s congaPos 47
execute as @e[type=creeper,scores={congaPos=47}] at @s if entity @e[type=creeper,scores={congaPos=46},distance=1..20] facing entity @e[type=creeper,scores={congaPos=46}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=47}] at @s unless entity @e[type=creeper,scores={congaPos=46},distance=..10] run scoreboard players set @s congaPos 46
execute at @e[type=creeper,scores={congaPos=47}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=48}] run scoreboard players set @s congaPos 48
execute as @e[type=creeper,scores={congaPos=48}] at @s if entity @e[type=creeper,scores={congaPos=47},distance=1..20] facing entity @e[type=creeper,scores={congaPos=47}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=48}] at @s unless entity @e[type=creeper,scores={congaPos=47},distance=..10] run scoreboard players set @s congaPos 47
execute at @e[type=creeper,scores={congaPos=48}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=49}] run scoreboard players set @s congaPos 49
execute as @e[type=creeper,scores={congaPos=49}] at @s if entity @e[type=creeper,scores={congaPos=48},distance=1..20] facing entity @e[type=creeper,scores={congaPos=48}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=49}] at @s unless entity @e[type=creeper,scores={congaPos=48},distance=..10] run scoreboard players set @s congaPos 48
execute at @e[type=creeper,scores={congaPos=49}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=50}] run scoreboard players set @s congaPos 50
execute as @e[type=creeper,scores={congaPos=50}] at @s if entity @e[type=creeper,scores={congaPos=49},distance=1..20] facing entity @e[type=creeper,scores={congaPos=49}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=50}] at @s unless entity @e[type=creeper,scores={congaPos=49},distance=..10] run scoreboard players set @s congaPos 49
execute at @e[type=creeper,scores={congaPos=50}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=51}] run scoreboard players set @s congaPos 51
execute as @e[type=creeper,scores={congaPos=51}] at @s if entity @e[type=creeper,scores={congaPos=50},distance=1..20] facing entity @e[type=creeper,scores={congaPos=50}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=51}] at @s unless entity @e[type=creeper,scores={congaPos=50},distance=..10] run scoreboard players set @s congaPos 50
execute at @e[type=creeper,scores={congaPos=51}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=52}] run scoreboard players set @s congaPos 52
execute as @e[type=creeper,scores={congaPos=52}] at @s if entity @e[type=creeper,scores={congaPos=51},distance=1..20] facing entity @e[type=creeper,scores={congaPos=51}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=52}] at @s unless entity @e[type=creeper,scores={congaPos=51},distance=..10] run scoreboard players set @s congaPos 51
execute at @e[type=creeper,scores={congaPos=52}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=53}] run scoreboard players set @s congaPos 53
execute as @e[type=creeper,scores={congaPos=53}] at @s if entity @e[type=creeper,scores={congaPos=52},distance=1..20] facing entity @e[type=creeper,scores={congaPos=52}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=53}] at @s unless entity @e[type=creeper,scores={congaPos=52},distance=..10] run scoreboard players set @s congaPos 52
execute at @e[type=creeper,scores={congaPos=53}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=54}] run scoreboard players set @s congaPos 54
execute as @e[type=creeper,scores={congaPos=54}] at @s if entity @e[type=creeper,scores={congaPos=53},distance=1..20] facing entity @e[type=creeper,scores={congaPos=53}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=54}] at @s unless entity @e[type=creeper,scores={congaPos=53},distance=..10] run scoreboard players set @s congaPos 53
execute at @e[type=creeper,scores={congaPos=54}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=55}] run scoreboard players set @s congaPos 55
execute as @e[type=creeper,scores={congaPos=55}] at @s if entity @e[type=creeper,scores={congaPos=54},distance=1..20] facing entity @e[type=creeper,scores={congaPos=54}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=55}] at @s unless entity @e[type=creeper,scores={congaPos=54},distance=..10] run scoreboard players set @s congaPos 54
execute at @e[type=creeper,scores={congaPos=55}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=56}] run scoreboard players set @s congaPos 56
execute as @e[type=creeper,scores={congaPos=56}] at @s if entity @e[type=creeper,scores={congaPos=55},distance=1..20] facing entity @e[type=creeper,scores={congaPos=55}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=56}] at @s unless entity @e[type=creeper,scores={congaPos=55},distance=..10] run scoreboard players set @s congaPos 55
execute at @e[type=creeper,scores={congaPos=56}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=57}] run scoreboard players set @s congaPos 57
execute as @e[type=creeper,scores={congaPos=57}] at @s if entity @e[type=creeper,scores={congaPos=56},distance=1..20] facing entity @e[type=creeper,scores={congaPos=56}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=57}] at @s unless entity @e[type=creeper,scores={congaPos=56},distance=..10] run scoreboard players set @s congaPos 56
execute at @e[type=creeper,scores={congaPos=57}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=58}] run scoreboard players set @s congaPos 58
execute as @e[type=creeper,scores={congaPos=58}] at @s if entity @e[type=creeper,scores={congaPos=57},distance=1..20] facing entity @e[type=creeper,scores={congaPos=57}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=58}] at @s unless entity @e[type=creeper,scores={congaPos=57},distance=..10] run scoreboard players set @s congaPos 57
execute at @e[type=creeper,scores={congaPos=58}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=59}] run scoreboard players set @s congaPos 59
execute as @e[type=creeper,scores={congaPos=59}] at @s if entity @e[type=creeper,scores={congaPos=58},distance=1..20] facing entity @e[type=creeper,scores={congaPos=58}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=59}] at @s unless entity @e[type=creeper,scores={congaPos=58},distance=..10] run scoreboard players set @s congaPos 58
execute at @e[type=creeper,scores={congaPos=59}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=60}] run scoreboard players set @s congaPos 60
execute as @e[type=creeper,scores={congaPos=60}] at @s if entity @e[type=creeper,scores={congaPos=59},distance=1..20] facing entity @e[type=creeper,scores={congaPos=59}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=60}] at @s unless entity @e[type=creeper,scores={congaPos=59},distance=..10] run scoreboard players set @s congaPos 59
execute at @e[type=creeper,scores={congaPos=60}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=61}] run scoreboard players set @s congaPos 61
execute as @e[type=creeper,scores={congaPos=61}] at @s if entity @e[type=creeper,scores={congaPos=60},distance=1..20] facing entity @e[type=creeper,scores={congaPos=60}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=61}] at @s unless entity @e[type=creeper,scores={congaPos=60},distance=..10] run scoreboard players set @s congaPos 60
execute at @e[type=creeper,scores={congaPos=61}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=62}] run scoreboard players set @s congaPos 62
execute as @e[type=creeper,scores={congaPos=62}] at @s if entity @e[type=creeper,scores={congaPos=61},distance=1..20] facing entity @e[type=creeper,scores={congaPos=61}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=62}] at @s unless entity @e[type=creeper,scores={congaPos=61},distance=..10] run scoreboard players set @s congaPos 61
execute at @e[type=creeper,scores={congaPos=62}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=63}] run scoreboard players set @s congaPos 63
execute as @e[type=creeper,scores={congaPos=63}] at @s if entity @e[type=creeper,scores={congaPos=62},distance=1..20] facing entity @e[type=creeper,scores={congaPos=62}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=63}] at @s unless entity @e[type=creeper,scores={congaPos=62},distance=..10] run scoreboard players set @s congaPos 62
execute at @e[type=creeper,scores={congaPos=63}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=64}] run scoreboard players set @s congaPos 64
execute as @e[type=creeper,scores={congaPos=64}] at @s if entity @e[type=creeper,scores={congaPos=63},distance=1..20] facing entity @e[type=creeper,scores={congaPos=63}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=64}] at @s unless entity @e[type=creeper,scores={congaPos=63},distance=..10] run scoreboard players set @s congaPos 63
execute at @e[type=creeper,scores={congaPos=64}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=65}] run scoreboard players set @s congaPos 65
execute as @e[type=creeper,scores={congaPos=65}] at @s if entity @e[type=creeper,scores={congaPos=64},distance=1..20] facing entity @e[type=creeper,scores={congaPos=64}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=65}] at @s unless entity @e[type=creeper,scores={congaPos=64},distance=..10] run scoreboard players set @s congaPos 64
execute at @e[type=creeper,scores={congaPos=65}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=66}] run scoreboard players set @s congaPos 66
execute as @e[type=creeper,scores={congaPos=66}] at @s if entity @e[type=creeper,scores={congaPos=65},distance=1..20] facing entity @e[type=creeper,scores={congaPos=65}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=66}] at @s unless entity @e[type=creeper,scores={congaPos=65},distance=..10] run scoreboard players set @s congaPos 65
execute at @e[type=creeper,scores={congaPos=66}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=67}] run scoreboard players set @s congaPos 67
execute as @e[type=creeper,scores={congaPos=67}] at @s if entity @e[type=creeper,scores={congaPos=66},distance=1..20] facing entity @e[type=creeper,scores={congaPos=66}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=67}] at @s unless entity @e[type=creeper,scores={congaPos=66},distance=..10] run scoreboard players set @s congaPos 66
execute at @e[type=creeper,scores={congaPos=67}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=68}] run scoreboard players set @s congaPos 68
execute as @e[type=creeper,scores={congaPos=68}] at @s if entity @e[type=creeper,scores={congaPos=67},distance=1..20] facing entity @e[type=creeper,scores={congaPos=67}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=68}] at @s unless entity @e[type=creeper,scores={congaPos=67},distance=..10] run scoreboard players set @s congaPos 67
execute at @e[type=creeper,scores={congaPos=68}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=69}] run scoreboard players set @s congaPos 69
execute as @e[type=creeper,scores={congaPos=69}] at @s if entity @e[type=creeper,scores={congaPos=68},distance=1..20] facing entity @e[type=creeper,scores={congaPos=68}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=69}] at @s unless entity @e[type=creeper,scores={congaPos=68},distance=..10] run scoreboard players set @s congaPos 68
execute at @e[type=creeper,scores={congaPos=69}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=70}] run scoreboard players set @s congaPos 70
execute as @e[type=creeper,scores={congaPos=70}] at @s if entity @e[type=creeper,scores={congaPos=69},distance=1..20] facing entity @e[type=creeper,scores={congaPos=69}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=70}] at @s unless entity @e[type=creeper,scores={congaPos=69},distance=..10] run scoreboard players set @s congaPos 69
execute at @e[type=creeper,scores={congaPos=70}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=71}] run scoreboard players set @s congaPos 71
execute as @e[type=creeper,scores={congaPos=71}] at @s if entity @e[type=creeper,scores={congaPos=70},distance=1..20] facing entity @e[type=creeper,scores={congaPos=70}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=71}] at @s unless entity @e[type=creeper,scores={congaPos=70},distance=..10] run scoreboard players set @s congaPos 70
execute at @e[type=creeper,scores={congaPos=71}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=72}] run scoreboard players set @s congaPos 72
execute as @e[type=creeper,scores={congaPos=72}] at @s if entity @e[type=creeper,scores={congaPos=71},distance=1..20] facing entity @e[type=creeper,scores={congaPos=71}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=72}] at @s unless entity @e[type=creeper,scores={congaPos=71},distance=..10] run scoreboard players set @s congaPos 71
execute at @e[type=creeper,scores={congaPos=72}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=73}] run scoreboard players set @s congaPos 73
execute as @e[type=creeper,scores={congaPos=73}] at @s if entity @e[type=creeper,scores={congaPos=72},distance=1..20] facing entity @e[type=creeper,scores={congaPos=72}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=73}] at @s unless entity @e[type=creeper,scores={congaPos=72},distance=..10] run scoreboard players set @s congaPos 72
execute at @e[type=creeper,scores={congaPos=73}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=74}] run scoreboard players set @s congaPos 74
execute as @e[type=creeper,scores={congaPos=74}] at @s if entity @e[type=creeper,scores={congaPos=73},distance=1..20] facing entity @e[type=creeper,scores={congaPos=73}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=74}] at @s unless entity @e[type=creeper,scores={congaPos=73},distance=..10] run scoreboard players set @s congaPos 73
execute at @e[type=creeper,scores={congaPos=74}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=75}] run scoreboard players set @s congaPos 75
execute as @e[type=creeper,scores={congaPos=75}] at @s if entity @e[type=creeper,scores={congaPos=74},distance=1..20] facing entity @e[type=creeper,scores={congaPos=74}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=75}] at @s unless entity @e[type=creeper,scores={congaPos=74},distance=..10] run scoreboard players set @s congaPos 74
execute at @e[type=creeper,scores={congaPos=75}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=76}] run scoreboard players set @s congaPos 76
execute as @e[type=creeper,scores={congaPos=76}] at @s if entity @e[type=creeper,scores={congaPos=75},distance=1..20] facing entity @e[type=creeper,scores={congaPos=75}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=76}] at @s unless entity @e[type=creeper,scores={congaPos=75},distance=..10] run scoreboard players set @s congaPos 75
execute at @e[type=creeper,scores={congaPos=76}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=77}] run scoreboard players set @s congaPos 77
execute as @e[type=creeper,scores={congaPos=77}] at @s if entity @e[type=creeper,scores={congaPos=76},distance=1..20] facing entity @e[type=creeper,scores={congaPos=76}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=77}] at @s unless entity @e[type=creeper,scores={congaPos=76},distance=..10] run scoreboard players set @s congaPos 76
execute at @e[type=creeper,scores={congaPos=77}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=78}] run scoreboard players set @s congaPos 78
execute as @e[type=creeper,scores={congaPos=78}] at @s if entity @e[type=creeper,scores={congaPos=77},distance=1..20] facing entity @e[type=creeper,scores={congaPos=77}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=78}] at @s unless entity @e[type=creeper,scores={congaPos=77},distance=..10] run scoreboard players set @s congaPos 77
execute at @e[type=creeper,scores={congaPos=78}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=79}] run scoreboard players set @s congaPos 79
execute as @e[type=creeper,scores={congaPos=79}] at @s if entity @e[type=creeper,scores={congaPos=78},distance=1..20] facing entity @e[type=creeper,scores={congaPos=78}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=79}] at @s unless entity @e[type=creeper,scores={congaPos=78},distance=..10] run scoreboard players set @s congaPos 78
execute at @e[type=creeper,scores={congaPos=79}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=80}] run scoreboard players set @s congaPos 80
execute as @e[type=creeper,scores={congaPos=80}] at @s if entity @e[type=creeper,scores={congaPos=79},distance=1..20] facing entity @e[type=creeper,scores={congaPos=79}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=80}] at @s unless entity @e[type=creeper,scores={congaPos=79},distance=..10] run scoreboard players set @s congaPos 79
execute at @e[type=creeper,scores={congaPos=80}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=81}] run scoreboard players set @s congaPos 81
execute as @e[type=creeper,scores={congaPos=81}] at @s if entity @e[type=creeper,scores={congaPos=80},distance=1..20] facing entity @e[type=creeper,scores={congaPos=80}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=81}] at @s unless entity @e[type=creeper,scores={congaPos=80},distance=..10] run scoreboard players set @s congaPos 80
execute at @e[type=creeper,scores={congaPos=81}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=82}] run scoreboard players set @s congaPos 82
execute as @e[type=creeper,scores={congaPos=82}] at @s if entity @e[type=creeper,scores={congaPos=81},distance=1..20] facing entity @e[type=creeper,scores={congaPos=81}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=82}] at @s unless entity @e[type=creeper,scores={congaPos=81},distance=..10] run scoreboard players set @s congaPos 81
execute at @e[type=creeper,scores={congaPos=82}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=83}] run scoreboard players set @s congaPos 83
execute as @e[type=creeper,scores={congaPos=83}] at @s if entity @e[type=creeper,scores={congaPos=82},distance=1..20] facing entity @e[type=creeper,scores={congaPos=82}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=83}] at @s unless entity @e[type=creeper,scores={congaPos=82},distance=..10] run scoreboard players set @s congaPos 82
execute at @e[type=creeper,scores={congaPos=83}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=84}] run scoreboard players set @s congaPos 84
execute as @e[type=creeper,scores={congaPos=84}] at @s if entity @e[type=creeper,scores={congaPos=83},distance=1..20] facing entity @e[type=creeper,scores={congaPos=83}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=84}] at @s unless entity @e[type=creeper,scores={congaPos=83},distance=..10] run scoreboard players set @s congaPos 83
execute at @e[type=creeper,scores={congaPos=84}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=85}] run scoreboard players set @s congaPos 85
execute as @e[type=creeper,scores={congaPos=85}] at @s if entity @e[type=creeper,scores={congaPos=84},distance=1..20] facing entity @e[type=creeper,scores={congaPos=84}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=85}] at @s unless entity @e[type=creeper,scores={congaPos=84},distance=..10] run scoreboard players set @s congaPos 84
execute at @e[type=creeper,scores={congaPos=85}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=86}] run scoreboard players set @s congaPos 86
execute as @e[type=creeper,scores={congaPos=86}] at @s if entity @e[type=creeper,scores={congaPos=85},distance=1..20] facing entity @e[type=creeper,scores={congaPos=85}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=86}] at @s unless entity @e[type=creeper,scores={congaPos=85},distance=..10] run scoreboard players set @s congaPos 85
execute at @e[type=creeper,scores={congaPos=86}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=87}] run scoreboard players set @s congaPos 87
execute as @e[type=creeper,scores={congaPos=87}] at @s if entity @e[type=creeper,scores={congaPos=86},distance=1..20] facing entity @e[type=creeper,scores={congaPos=86}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=87}] at @s unless entity @e[type=creeper,scores={congaPos=86},distance=..10] run scoreboard players set @s congaPos 86
execute at @e[type=creeper,scores={congaPos=87}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=88}] run scoreboard players set @s congaPos 88
execute as @e[type=creeper,scores={congaPos=88}] at @s if entity @e[type=creeper,scores={congaPos=87},distance=1..20] facing entity @e[type=creeper,scores={congaPos=87}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=88}] at @s unless entity @e[type=creeper,scores={congaPos=87},distance=..10] run scoreboard players set @s congaPos 87
execute at @e[type=creeper,scores={congaPos=88}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=89}] run scoreboard players set @s congaPos 89
execute as @e[type=creeper,scores={congaPos=89}] at @s if entity @e[type=creeper,scores={congaPos=88},distance=1..20] facing entity @e[type=creeper,scores={congaPos=88}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=89}] at @s unless entity @e[type=creeper,scores={congaPos=88},distance=..10] run scoreboard players set @s congaPos 88
execute at @e[type=creeper,scores={congaPos=89}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=90}] run scoreboard players set @s congaPos 90
execute as @e[type=creeper,scores={congaPos=90}] at @s if entity @e[type=creeper,scores={congaPos=89},distance=1..20] facing entity @e[type=creeper,scores={congaPos=89}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=90}] at @s unless entity @e[type=creeper,scores={congaPos=89},distance=..10] run scoreboard players set @s congaPos 89
execute at @e[type=creeper,scores={congaPos=90}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=91}] run scoreboard players set @s congaPos 91
execute as @e[type=creeper,scores={congaPos=91}] at @s if entity @e[type=creeper,scores={congaPos=90},distance=1..20] facing entity @e[type=creeper,scores={congaPos=90}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=91}] at @s unless entity @e[type=creeper,scores={congaPos=90},distance=..10] run scoreboard players set @s congaPos 90
execute at @e[type=creeper,scores={congaPos=91}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=92}] run scoreboard players set @s congaPos 92
execute as @e[type=creeper,scores={congaPos=92}] at @s if entity @e[type=creeper,scores={congaPos=91},distance=1..20] facing entity @e[type=creeper,scores={congaPos=91}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=92}] at @s unless entity @e[type=creeper,scores={congaPos=91},distance=..10] run scoreboard players set @s congaPos 91
execute at @e[type=creeper,scores={congaPos=92}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=93}] run scoreboard players set @s congaPos 93
execute as @e[type=creeper,scores={congaPos=93}] at @s if entity @e[type=creeper,scores={congaPos=92},distance=1..20] facing entity @e[type=creeper,scores={congaPos=92}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=93}] at @s unless entity @e[type=creeper,scores={congaPos=92},distance=..10] run scoreboard players set @s congaPos 92
execute at @e[type=creeper,scores={congaPos=93}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=94}] run scoreboard players set @s congaPos 94
execute as @e[type=creeper,scores={congaPos=94}] at @s if entity @e[type=creeper,scores={congaPos=93},distance=1..20] facing entity @e[type=creeper,scores={congaPos=93}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=94}] at @s unless entity @e[type=creeper,scores={congaPos=93},distance=..10] run scoreboard players set @s congaPos 93
execute at @e[type=creeper,scores={congaPos=94}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=95}] run scoreboard players set @s congaPos 95
execute as @e[type=creeper,scores={congaPos=95}] at @s if entity @e[type=creeper,scores={congaPos=94},distance=1..20] facing entity @e[type=creeper,scores={congaPos=94}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=95}] at @s unless entity @e[type=creeper,scores={congaPos=94},distance=..10] run scoreboard players set @s congaPos 94
execute at @e[type=creeper,scores={congaPos=95}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=96}] run scoreboard players set @s congaPos 96
execute as @e[type=creeper,scores={congaPos=96}] at @s if entity @e[type=creeper,scores={congaPos=95},distance=1..20] facing entity @e[type=creeper,scores={congaPos=95}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=96}] at @s unless entity @e[type=creeper,scores={congaPos=95},distance=..10] run scoreboard players set @s congaPos 95
execute at @e[type=creeper,scores={congaPos=96}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=97}] run scoreboard players set @s congaPos 97
execute as @e[type=creeper,scores={congaPos=97}] at @s if entity @e[type=creeper,scores={congaPos=96},distance=1..20] facing entity @e[type=creeper,scores={congaPos=96}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=97}] at @s unless entity @e[type=creeper,scores={congaPos=96},distance=..10] run scoreboard players set @s congaPos 96
execute at @e[type=creeper,scores={congaPos=97}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=98}] run scoreboard players set @s congaPos 98
execute as @e[type=creeper,scores={congaPos=98}] at @s if entity @e[type=creeper,scores={congaPos=97},distance=1..20] facing entity @e[type=creeper,scores={congaPos=97}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=98}] at @s unless entity @e[type=creeper,scores={congaPos=97},distance=..10] run scoreboard players set @s congaPos 97
execute at @e[type=creeper,scores={congaPos=98}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=99}] run scoreboard players set @s congaPos 99
execute as @e[type=creeper,scores={congaPos=99}] at @s if entity @e[type=creeper,scores={congaPos=98},distance=1..20] facing entity @e[type=creeper,scores={congaPos=98}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=99}] at @s unless entity @e[type=creeper,scores={congaPos=98},distance=..10] run scoreboard players set @s congaPos 98
execute at @e[type=creeper,scores={congaPos=99}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos=100}] run scoreboard players set @s congaPos 100
execute as @e[type=creeper,scores={congaPos=100}] at @s if entity @e[type=creeper,scores={congaPos=99},distance=1..20] facing entity @e[type=creeper,scores={congaPos=99}] feet run teleport ^ ^ ^0.15
execute as @e[type=creeper,scores={congaPos=100}] at @s unless entity @e[type=creeper,scores={congaPos=99},distance=..10] run scoreboard players set @s congaPos 99
#Speed buff for the front
effect give @e[type=creeper,scores={congaPos=1}] speed 1 2 true
#Disables AI of any that are not the front
execute as @e[type=creeper,scores={congaPos=2..}] run data merge entity @s {NoAI:1b}
execute as @e[type=creeper,scores={congaPos=..1}] run data merge entity @s {NoAI:0b}
#Allows back members to explode
execute as @e[type=creeper,scores={congaPos=2..}] at @s if entity @a[distance=..2] run data merge entity @s {ignited:1b}
#Marker/reset
tag @e[type=creeper,scores={congaPos=1..}] add inconga
scoreboard players set @e[type=creeper,tag=!inconga] congaPos 0
