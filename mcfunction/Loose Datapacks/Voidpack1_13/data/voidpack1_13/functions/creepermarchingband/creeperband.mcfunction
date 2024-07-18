#Marks the front of the conga line
execute unless entity @e[type=creeper,scores={glowTimer=1},distance=..80] at @a run scoreboard players set @e[type=creeper,limit=1,sort=nearest] glowTimer 1
#Sets position/moves conga line parts/corrects line position
execute as @e[type=creeper,scores={glowTimer=1}] at @s if entity @p[distance=1..20] facing entity @p feet run teleport ^ ^ ^0.25
execute at @e[type=creeper,scores={glowTimer=1}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=2}] run scoreboard players set @s glowTimer 2
execute as @e[type=creeper,scores={glowTimer=2}] at @s if entity @e[type=creeper,scores={glowTimer=1},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=1}] feet run teleport ^ ^ ^0.25
execute at @e[type=creeper,scores={glowTimer=2}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=3}] run scoreboard players set @s glowTimer 3
execute as @e[type=creeper,scores={glowTimer=3}] at @s if entity @e[type=creeper,scores={glowTimer=2},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=2}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=3}] at @s unless entity @e[type=creeper,scores={glowTimer=2},distance=..10] run scoreboard players set @s glowTimer 2
execute at @e[type=creeper,scores={glowTimer=3}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=4}] run scoreboard players set @s glowTimer 4
execute as @e[type=creeper,scores={glowTimer=4}] at @s if entity @e[type=creeper,scores={glowTimer=3},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=3}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=4}] at @s unless entity @e[type=creeper,scores={glowTimer=3},distance=..10] run scoreboard players set @s glowTimer 3
execute at @e[type=creeper,scores={glowTimer=4}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=5}] run scoreboard players set @s glowTimer 5
execute as @e[type=creeper,scores={glowTimer=5}] at @s if entity @e[type=creeper,scores={glowTimer=4},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=4}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=5}] at @s unless entity @e[type=creeper,scores={glowTimer=4},distance=..10] run scoreboard players set @s glowTimer 4
execute at @e[type=creeper,scores={glowTimer=5}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=6}] run scoreboard players set @s glowTimer 6
execute as @e[type=creeper,scores={glowTimer=6}] at @s if entity @e[type=creeper,scores={glowTimer=5},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=5}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=6}] at @s unless entity @e[type=creeper,scores={glowTimer=5},distance=..10] run scoreboard players set @s glowTimer 5
execute at @e[type=creeper,scores={glowTimer=6}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=7}] run scoreboard players set @s glowTimer 7
execute as @e[type=creeper,scores={glowTimer=7}] at @s if entity @e[type=creeper,scores={glowTimer=6},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=6}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=7}] at @s unless entity @e[type=creeper,scores={glowTimer=6},distance=..10] run scoreboard players set @s glowTimer 6
execute at @e[type=creeper,scores={glowTimer=7}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=8}] run scoreboard players set @s glowTimer 8
execute as @e[type=creeper,scores={glowTimer=8}] at @s if entity @e[type=creeper,scores={glowTimer=7},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=7}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=8}] at @s unless entity @e[type=creeper,scores={glowTimer=7},distance=..10] run scoreboard players set @s glowTimer 7
execute at @e[type=creeper,scores={glowTimer=8}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=9}] run scoreboard players set @s glowTimer 9
execute as @e[type=creeper,scores={glowTimer=9}] at @s if entity @e[type=creeper,scores={glowTimer=8},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=8}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=9}] at @s unless entity @e[type=creeper,scores={glowTimer=8},distance=..10] run scoreboard players set @s glowTimer 8
execute at @e[type=creeper,scores={glowTimer=9}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=10}] run scoreboard players set @s glowTimer 10
execute as @e[type=creeper,scores={glowTimer=10}] at @s if entity @e[type=creeper,scores={glowTimer=9},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=9}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=10}] at @s unless entity @e[type=creeper,scores={glowTimer=9},distance=..10] run scoreboard players set @s glowTimer 9
execute at @e[type=creeper,scores={glowTimer=10}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=11}] run scoreboard players set @s glowTimer 11
execute as @e[type=creeper,scores={glowTimer=11}] at @s if entity @e[type=creeper,scores={glowTimer=10},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=10}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=11}] at @s unless entity @e[type=creeper,scores={glowTimer=10},distance=..10] run scoreboard players set @s glowTimer 10
execute at @e[type=creeper,scores={glowTimer=11}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=12}] run scoreboard players set @s glowTimer 12
execute as @e[type=creeper,scores={glowTimer=12}] at @s if entity @e[type=creeper,scores={glowTimer=11},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=11}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=12}] at @s unless entity @e[type=creeper,scores={glowTimer=11},distance=..10] run scoreboard players set @s glowTimer 11
execute at @e[type=creeper,scores={glowTimer=12}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=13}] run scoreboard players set @s glowTimer 13
execute as @e[type=creeper,scores={glowTimer=13}] at @s if entity @e[type=creeper,scores={glowTimer=12},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=12}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=13}] at @s unless entity @e[type=creeper,scores={glowTimer=12},distance=..10] run scoreboard players set @s glowTimer 12
execute at @e[type=creeper,scores={glowTimer=13}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=14}] run scoreboard players set @s glowTimer 14
execute as @e[type=creeper,scores={glowTimer=14}] at @s if entity @e[type=creeper,scores={glowTimer=13},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=13}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=14}] at @s unless entity @e[type=creeper,scores={glowTimer=13},distance=..10] run scoreboard players set @s glowTimer 13
execute at @e[type=creeper,scores={glowTimer=14}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=15}] run scoreboard players set @s glowTimer 15
execute as @e[type=creeper,scores={glowTimer=15}] at @s if entity @e[type=creeper,scores={glowTimer=14},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=14}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=15}] at @s unless entity @e[type=creeper,scores={glowTimer=14},distance=..10] run scoreboard players set @s glowTimer 14
execute at @e[type=creeper,scores={glowTimer=15}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=16}] run scoreboard players set @s glowTimer 16
execute as @e[type=creeper,scores={glowTimer=16}] at @s if entity @e[type=creeper,scores={glowTimer=15},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=15}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=16}] at @s unless entity @e[type=creeper,scores={glowTimer=15},distance=..10] run scoreboard players set @s glowTimer 15
execute at @e[type=creeper,scores={glowTimer=16}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=17}] run scoreboard players set @s glowTimer 17
execute as @e[type=creeper,scores={glowTimer=17}] at @s if entity @e[type=creeper,scores={glowTimer=16},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=16}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=17}] at @s unless entity @e[type=creeper,scores={glowTimer=16},distance=..10] run scoreboard players set @s glowTimer 16
execute at @e[type=creeper,scores={glowTimer=17}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=18}] run scoreboard players set @s glowTimer 18
execute as @e[type=creeper,scores={glowTimer=18}] at @s if entity @e[type=creeper,scores={glowTimer=17},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=17}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=18}] at @s unless entity @e[type=creeper,scores={glowTimer=17},distance=..10] run scoreboard players set @s glowTimer 17
execute at @e[type=creeper,scores={glowTimer=18}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=19}] run scoreboard players set @s glowTimer 19
execute as @e[type=creeper,scores={glowTimer=19}] at @s if entity @e[type=creeper,scores={glowTimer=18},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=18}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=19}] at @s unless entity @e[type=creeper,scores={glowTimer=18},distance=..10] run scoreboard players set @s glowTimer 18
execute at @e[type=creeper,scores={glowTimer=19}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=20}] run scoreboard players set @s glowTimer 20
execute as @e[type=creeper,scores={glowTimer=20}] at @s if entity @e[type=creeper,scores={glowTimer=19},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=19}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=20}] at @s unless entity @e[type=creeper,scores={glowTimer=19},distance=..10] run scoreboard players set @s glowTimer 19
execute at @e[type=creeper,scores={glowTimer=20}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=21}] run scoreboard players set @s glowTimer 21
execute as @e[type=creeper,scores={glowTimer=21}] at @s if entity @e[type=creeper,scores={glowTimer=20},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=20}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=21}] at @s unless entity @e[type=creeper,scores={glowTimer=20},distance=..10] run scoreboard players set @s glowTimer 20
execute at @e[type=creeper,scores={glowTimer=21}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=22}] run scoreboard players set @s glowTimer 22
execute as @e[type=creeper,scores={glowTimer=22}] at @s if entity @e[type=creeper,scores={glowTimer=21},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=21}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=22}] at @s unless entity @e[type=creeper,scores={glowTimer=21},distance=..10] run scoreboard players set @s glowTimer 21
execute at @e[type=creeper,scores={glowTimer=22}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=23}] run scoreboard players set @s glowTimer 23
execute as @e[type=creeper,scores={glowTimer=23}] at @s if entity @e[type=creeper,scores={glowTimer=22},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=22}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=23}] at @s unless entity @e[type=creeper,scores={glowTimer=22},distance=..10] run scoreboard players set @s glowTimer 22
execute at @e[type=creeper,scores={glowTimer=23}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=24}] run scoreboard players set @s glowTimer 24
execute as @e[type=creeper,scores={glowTimer=24}] at @s if entity @e[type=creeper,scores={glowTimer=23},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=23}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=24}] at @s unless entity @e[type=creeper,scores={glowTimer=23},distance=..10] run scoreboard players set @s glowTimer 23
execute at @e[type=creeper,scores={glowTimer=24}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=25}] run scoreboard players set @s glowTimer 25
execute as @e[type=creeper,scores={glowTimer=25}] at @s if entity @e[type=creeper,scores={glowTimer=24},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=24}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=25}] at @s unless entity @e[type=creeper,scores={glowTimer=24},distance=..10] run scoreboard players set @s glowTimer 24
execute at @e[type=creeper,scores={glowTimer=25}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=26}] run scoreboard players set @s glowTimer 26
execute as @e[type=creeper,scores={glowTimer=26}] at @s if entity @e[type=creeper,scores={glowTimer=25},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=25}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=26}] at @s unless entity @e[type=creeper,scores={glowTimer=25},distance=..10] run scoreboard players set @s glowTimer 25
execute at @e[type=creeper,scores={glowTimer=26}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=27}] run scoreboard players set @s glowTimer 27
execute as @e[type=creeper,scores={glowTimer=27}] at @s if entity @e[type=creeper,scores={glowTimer=26},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=26}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=27}] at @s unless entity @e[type=creeper,scores={glowTimer=26},distance=..10] run scoreboard players set @s glowTimer 26
execute at @e[type=creeper,scores={glowTimer=27}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=28}] run scoreboard players set @s glowTimer 28
execute as @e[type=creeper,scores={glowTimer=28}] at @s if entity @e[type=creeper,scores={glowTimer=27},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=27}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=28}] at @s unless entity @e[type=creeper,scores={glowTimer=27},distance=..10] run scoreboard players set @s glowTimer 27
execute at @e[type=creeper,scores={glowTimer=28}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=29}] run scoreboard players set @s glowTimer 29
execute as @e[type=creeper,scores={glowTimer=29}] at @s if entity @e[type=creeper,scores={glowTimer=28},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=28}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=29}] at @s unless entity @e[type=creeper,scores={glowTimer=28},distance=..10] run scoreboard players set @s glowTimer 28
execute at @e[type=creeper,scores={glowTimer=29}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=30}] run scoreboard players set @s glowTimer 30
execute as @e[type=creeper,scores={glowTimer=30}] at @s if entity @e[type=creeper,scores={glowTimer=29},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=29}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=30}] at @s unless entity @e[type=creeper,scores={glowTimer=29},distance=..10] run scoreboard players set @s glowTimer 29
execute at @e[type=creeper,scores={glowTimer=30}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=31}] run scoreboard players set @s glowTimer 31
execute as @e[type=creeper,scores={glowTimer=31}] at @s if entity @e[type=creeper,scores={glowTimer=30},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=30}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=31}] at @s unless entity @e[type=creeper,scores={glowTimer=30},distance=..10] run scoreboard players set @s glowTimer 30
execute at @e[type=creeper,scores={glowTimer=31}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=32}] run scoreboard players set @s glowTimer 32
execute as @e[type=creeper,scores={glowTimer=32}] at @s if entity @e[type=creeper,scores={glowTimer=31},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=31}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=32}] at @s unless entity @e[type=creeper,scores={glowTimer=31},distance=..10] run scoreboard players set @s glowTimer 31
execute at @e[type=creeper,scores={glowTimer=32}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=33}] run scoreboard players set @s glowTimer 33
execute as @e[type=creeper,scores={glowTimer=33}] at @s if entity @e[type=creeper,scores={glowTimer=32},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=32}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=33}] at @s unless entity @e[type=creeper,scores={glowTimer=32},distance=..10] run scoreboard players set @s glowTimer 32
execute at @e[type=creeper,scores={glowTimer=33}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=34}] run scoreboard players set @s glowTimer 34
execute as @e[type=creeper,scores={glowTimer=34}] at @s if entity @e[type=creeper,scores={glowTimer=33},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=33}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=34}] at @s unless entity @e[type=creeper,scores={glowTimer=33},distance=..10] run scoreboard players set @s glowTimer 33
execute at @e[type=creeper,scores={glowTimer=34}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=35}] run scoreboard players set @s glowTimer 35
execute as @e[type=creeper,scores={glowTimer=35}] at @s if entity @e[type=creeper,scores={glowTimer=34},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=34}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=35}] at @s unless entity @e[type=creeper,scores={glowTimer=34},distance=..10] run scoreboard players set @s glowTimer 34
execute at @e[type=creeper,scores={glowTimer=35}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=36}] run scoreboard players set @s glowTimer 36
execute as @e[type=creeper,scores={glowTimer=36}] at @s if entity @e[type=creeper,scores={glowTimer=35},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=35}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=36}] at @s unless entity @e[type=creeper,scores={glowTimer=35},distance=..10] run scoreboard players set @s glowTimer 35
execute at @e[type=creeper,scores={glowTimer=36}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=37}] run scoreboard players set @s glowTimer 37
execute as @e[type=creeper,scores={glowTimer=37}] at @s if entity @e[type=creeper,scores={glowTimer=36},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=36}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=37}] at @s unless entity @e[type=creeper,scores={glowTimer=36},distance=..10] run scoreboard players set @s glowTimer 36
execute at @e[type=creeper,scores={glowTimer=37}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=38}] run scoreboard players set @s glowTimer 38
execute as @e[type=creeper,scores={glowTimer=38}] at @s if entity @e[type=creeper,scores={glowTimer=37},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=37}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=38}] at @s unless entity @e[type=creeper,scores={glowTimer=37},distance=..10] run scoreboard players set @s glowTimer 37
execute at @e[type=creeper,scores={glowTimer=38}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=39}] run scoreboard players set @s glowTimer 39
execute as @e[type=creeper,scores={glowTimer=39}] at @s if entity @e[type=creeper,scores={glowTimer=38},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=38}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=39}] at @s unless entity @e[type=creeper,scores={glowTimer=38},distance=..10] run scoreboard players set @s glowTimer 38
execute at @e[type=creeper,scores={glowTimer=39}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=40}] run scoreboard players set @s glowTimer 40
execute as @e[type=creeper,scores={glowTimer=40}] at @s if entity @e[type=creeper,scores={glowTimer=39},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=39}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=40}] at @s unless entity @e[type=creeper,scores={glowTimer=39},distance=..10] run scoreboard players set @s glowTimer 39
execute at @e[type=creeper,scores={glowTimer=40}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=41}] run scoreboard players set @s glowTimer 41
execute as @e[type=creeper,scores={glowTimer=41}] at @s if entity @e[type=creeper,scores={glowTimer=40},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=40}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=41}] at @s unless entity @e[type=creeper,scores={glowTimer=40},distance=..10] run scoreboard players set @s glowTimer 40
execute at @e[type=creeper,scores={glowTimer=41}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=42}] run scoreboard players set @s glowTimer 42
execute as @e[type=creeper,scores={glowTimer=42}] at @s if entity @e[type=creeper,scores={glowTimer=41},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=41}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=42}] at @s unless entity @e[type=creeper,scores={glowTimer=41},distance=..10] run scoreboard players set @s glowTimer 41
execute at @e[type=creeper,scores={glowTimer=42}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=43}] run scoreboard players set @s glowTimer 43
execute as @e[type=creeper,scores={glowTimer=43}] at @s if entity @e[type=creeper,scores={glowTimer=42},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=42}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=43}] at @s unless entity @e[type=creeper,scores={glowTimer=42},distance=..10] run scoreboard players set @s glowTimer 42
execute at @e[type=creeper,scores={glowTimer=43}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=44}] run scoreboard players set @s glowTimer 44
execute as @e[type=creeper,scores={glowTimer=44}] at @s if entity @e[type=creeper,scores={glowTimer=43},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=43}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=44}] at @s unless entity @e[type=creeper,scores={glowTimer=43},distance=..10] run scoreboard players set @s glowTimer 43
execute at @e[type=creeper,scores={glowTimer=44}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=45}] run scoreboard players set @s glowTimer 45
execute as @e[type=creeper,scores={glowTimer=45}] at @s if entity @e[type=creeper,scores={glowTimer=44},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=44}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=45}] at @s unless entity @e[type=creeper,scores={glowTimer=44},distance=..10] run scoreboard players set @s glowTimer 44
execute at @e[type=creeper,scores={glowTimer=45}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=46}] run scoreboard players set @s glowTimer 46
execute as @e[type=creeper,scores={glowTimer=46}] at @s if entity @e[type=creeper,scores={glowTimer=45},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=45}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=46}] at @s unless entity @e[type=creeper,scores={glowTimer=45},distance=..10] run scoreboard players set @s glowTimer 45
execute at @e[type=creeper,scores={glowTimer=46}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=47}] run scoreboard players set @s glowTimer 47
execute as @e[type=creeper,scores={glowTimer=47}] at @s if entity @e[type=creeper,scores={glowTimer=46},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=46}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=47}] at @s unless entity @e[type=creeper,scores={glowTimer=46},distance=..10] run scoreboard players set @s glowTimer 46
execute at @e[type=creeper,scores={glowTimer=47}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=48}] run scoreboard players set @s glowTimer 48
execute as @e[type=creeper,scores={glowTimer=48}] at @s if entity @e[type=creeper,scores={glowTimer=47},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=47}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=48}] at @s unless entity @e[type=creeper,scores={glowTimer=47},distance=..10] run scoreboard players set @s glowTimer 47
execute at @e[type=creeper,scores={glowTimer=48}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=49}] run scoreboard players set @s glowTimer 49
execute as @e[type=creeper,scores={glowTimer=49}] at @s if entity @e[type=creeper,scores={glowTimer=48},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=48}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=49}] at @s unless entity @e[type=creeper,scores={glowTimer=48},distance=..10] run scoreboard players set @s glowTimer 48
execute at @e[type=creeper,scores={glowTimer=49}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=50}] run scoreboard players set @s glowTimer 50
execute as @e[type=creeper,scores={glowTimer=50}] at @s if entity @e[type=creeper,scores={glowTimer=49},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=49}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=50}] at @s unless entity @e[type=creeper,scores={glowTimer=49},distance=..10] run scoreboard players set @s glowTimer 49
execute at @e[type=creeper,scores={glowTimer=50}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=51}] run scoreboard players set @s glowTimer 51
execute as @e[type=creeper,scores={glowTimer=51}] at @s if entity @e[type=creeper,scores={glowTimer=50},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=50}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=51}] at @s unless entity @e[type=creeper,scores={glowTimer=50},distance=..10] run scoreboard players set @s glowTimer 50
execute at @e[type=creeper,scores={glowTimer=51}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=52}] run scoreboard players set @s glowTimer 52
execute as @e[type=creeper,scores={glowTimer=52}] at @s if entity @e[type=creeper,scores={glowTimer=51},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=51}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=52}] at @s unless entity @e[type=creeper,scores={glowTimer=51},distance=..10] run scoreboard players set @s glowTimer 51
execute at @e[type=creeper,scores={glowTimer=52}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=53}] run scoreboard players set @s glowTimer 53
execute as @e[type=creeper,scores={glowTimer=53}] at @s if entity @e[type=creeper,scores={glowTimer=52},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=52}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=53}] at @s unless entity @e[type=creeper,scores={glowTimer=52},distance=..10] run scoreboard players set @s glowTimer 52
execute at @e[type=creeper,scores={glowTimer=53}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=54}] run scoreboard players set @s glowTimer 54
execute as @e[type=creeper,scores={glowTimer=54}] at @s if entity @e[type=creeper,scores={glowTimer=53},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=53}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=54}] at @s unless entity @e[type=creeper,scores={glowTimer=53},distance=..10] run scoreboard players set @s glowTimer 53
execute at @e[type=creeper,scores={glowTimer=54}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=55}] run scoreboard players set @s glowTimer 55
execute as @e[type=creeper,scores={glowTimer=55}] at @s if entity @e[type=creeper,scores={glowTimer=54},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=54}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=55}] at @s unless entity @e[type=creeper,scores={glowTimer=54},distance=..10] run scoreboard players set @s glowTimer 54
execute at @e[type=creeper,scores={glowTimer=55}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=56}] run scoreboard players set @s glowTimer 56
execute as @e[type=creeper,scores={glowTimer=56}] at @s if entity @e[type=creeper,scores={glowTimer=55},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=55}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=56}] at @s unless entity @e[type=creeper,scores={glowTimer=55},distance=..10] run scoreboard players set @s glowTimer 55
execute at @e[type=creeper,scores={glowTimer=56}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=57}] run scoreboard players set @s glowTimer 57
execute as @e[type=creeper,scores={glowTimer=57}] at @s if entity @e[type=creeper,scores={glowTimer=56},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=56}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=57}] at @s unless entity @e[type=creeper,scores={glowTimer=56},distance=..10] run scoreboard players set @s glowTimer 56
execute at @e[type=creeper,scores={glowTimer=57}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=58}] run scoreboard players set @s glowTimer 58
execute as @e[type=creeper,scores={glowTimer=58}] at @s if entity @e[type=creeper,scores={glowTimer=57},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=57}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=58}] at @s unless entity @e[type=creeper,scores={glowTimer=57},distance=..10] run scoreboard players set @s glowTimer 57
execute at @e[type=creeper,scores={glowTimer=58}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=59}] run scoreboard players set @s glowTimer 59
execute as @e[type=creeper,scores={glowTimer=59}] at @s if entity @e[type=creeper,scores={glowTimer=58},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=58}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=59}] at @s unless entity @e[type=creeper,scores={glowTimer=58},distance=..10] run scoreboard players set @s glowTimer 58
execute at @e[type=creeper,scores={glowTimer=59}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=60}] run scoreboard players set @s glowTimer 60
execute as @e[type=creeper,scores={glowTimer=60}] at @s if entity @e[type=creeper,scores={glowTimer=59},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=59}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=60}] at @s unless entity @e[type=creeper,scores={glowTimer=59},distance=..10] run scoreboard players set @s glowTimer 59
execute at @e[type=creeper,scores={glowTimer=60}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=61}] run scoreboard players set @s glowTimer 61
execute as @e[type=creeper,scores={glowTimer=61}] at @s if entity @e[type=creeper,scores={glowTimer=60},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=60}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=61}] at @s unless entity @e[type=creeper,scores={glowTimer=60},distance=..10] run scoreboard players set @s glowTimer 60
execute at @e[type=creeper,scores={glowTimer=61}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=62}] run scoreboard players set @s glowTimer 62
execute as @e[type=creeper,scores={glowTimer=62}] at @s if entity @e[type=creeper,scores={glowTimer=61},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=61}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=62}] at @s unless entity @e[type=creeper,scores={glowTimer=61},distance=..10] run scoreboard players set @s glowTimer 61
execute at @e[type=creeper,scores={glowTimer=62}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=63}] run scoreboard players set @s glowTimer 63
execute as @e[type=creeper,scores={glowTimer=63}] at @s if entity @e[type=creeper,scores={glowTimer=62},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=62}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=63}] at @s unless entity @e[type=creeper,scores={glowTimer=62},distance=..10] run scoreboard players set @s glowTimer 62
execute at @e[type=creeper,scores={glowTimer=63}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=64}] run scoreboard players set @s glowTimer 64
execute as @e[type=creeper,scores={glowTimer=64}] at @s if entity @e[type=creeper,scores={glowTimer=63},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=63}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=64}] at @s unless entity @e[type=creeper,scores={glowTimer=63},distance=..10] run scoreboard players set @s glowTimer 63
execute at @e[type=creeper,scores={glowTimer=64}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=65}] run scoreboard players set @s glowTimer 65
execute as @e[type=creeper,scores={glowTimer=65}] at @s if entity @e[type=creeper,scores={glowTimer=64},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=64}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=65}] at @s unless entity @e[type=creeper,scores={glowTimer=64},distance=..10] run scoreboard players set @s glowTimer 64
execute at @e[type=creeper,scores={glowTimer=65}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=66}] run scoreboard players set @s glowTimer 66
execute as @e[type=creeper,scores={glowTimer=66}] at @s if entity @e[type=creeper,scores={glowTimer=65},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=65}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=66}] at @s unless entity @e[type=creeper,scores={glowTimer=65},distance=..10] run scoreboard players set @s glowTimer 65
execute at @e[type=creeper,scores={glowTimer=66}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=67}] run scoreboard players set @s glowTimer 67
execute as @e[type=creeper,scores={glowTimer=67}] at @s if entity @e[type=creeper,scores={glowTimer=66},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=66}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=67}] at @s unless entity @e[type=creeper,scores={glowTimer=66},distance=..10] run scoreboard players set @s glowTimer 66
execute at @e[type=creeper,scores={glowTimer=67}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=68}] run scoreboard players set @s glowTimer 68
execute as @e[type=creeper,scores={glowTimer=68}] at @s if entity @e[type=creeper,scores={glowTimer=67},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=67}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=68}] at @s unless entity @e[type=creeper,scores={glowTimer=67},distance=..10] run scoreboard players set @s glowTimer 67
execute at @e[type=creeper,scores={glowTimer=68}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=69}] run scoreboard players set @s glowTimer 69
execute as @e[type=creeper,scores={glowTimer=69}] at @s if entity @e[type=creeper,scores={glowTimer=68},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=68}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=69}] at @s unless entity @e[type=creeper,scores={glowTimer=68},distance=..10] run scoreboard players set @s glowTimer 68
execute at @e[type=creeper,scores={glowTimer=69}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=70}] run scoreboard players set @s glowTimer 70
execute as @e[type=creeper,scores={glowTimer=70}] at @s if entity @e[type=creeper,scores={glowTimer=69},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=69}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=70}] at @s unless entity @e[type=creeper,scores={glowTimer=69},distance=..10] run scoreboard players set @s glowTimer 69
execute at @e[type=creeper,scores={glowTimer=70}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=71}] run scoreboard players set @s glowTimer 71
execute as @e[type=creeper,scores={glowTimer=71}] at @s if entity @e[type=creeper,scores={glowTimer=70},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=70}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=71}] at @s unless entity @e[type=creeper,scores={glowTimer=70},distance=..10] run scoreboard players set @s glowTimer 70
execute at @e[type=creeper,scores={glowTimer=71}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=72}] run scoreboard players set @s glowTimer 72
execute as @e[type=creeper,scores={glowTimer=72}] at @s if entity @e[type=creeper,scores={glowTimer=71},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=71}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=72}] at @s unless entity @e[type=creeper,scores={glowTimer=71},distance=..10] run scoreboard players set @s glowTimer 71
execute at @e[type=creeper,scores={glowTimer=72}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=73}] run scoreboard players set @s glowTimer 73
execute as @e[type=creeper,scores={glowTimer=73}] at @s if entity @e[type=creeper,scores={glowTimer=72},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=72}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=73}] at @s unless entity @e[type=creeper,scores={glowTimer=72},distance=..10] run scoreboard players set @s glowTimer 72
execute at @e[type=creeper,scores={glowTimer=73}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=74}] run scoreboard players set @s glowTimer 74
execute as @e[type=creeper,scores={glowTimer=74}] at @s if entity @e[type=creeper,scores={glowTimer=73},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=73}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=74}] at @s unless entity @e[type=creeper,scores={glowTimer=73},distance=..10] run scoreboard players set @s glowTimer 73
execute at @e[type=creeper,scores={glowTimer=74}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=75}] run scoreboard players set @s glowTimer 75
execute as @e[type=creeper,scores={glowTimer=75}] at @s if entity @e[type=creeper,scores={glowTimer=74},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=74}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=75}] at @s unless entity @e[type=creeper,scores={glowTimer=74},distance=..10] run scoreboard players set @s glowTimer 74
execute at @e[type=creeper,scores={glowTimer=75}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=76}] run scoreboard players set @s glowTimer 76
execute as @e[type=creeper,scores={glowTimer=76}] at @s if entity @e[type=creeper,scores={glowTimer=75},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=75}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=76}] at @s unless entity @e[type=creeper,scores={glowTimer=75},distance=..10] run scoreboard players set @s glowTimer 75
execute at @e[type=creeper,scores={glowTimer=76}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=77}] run scoreboard players set @s glowTimer 77
execute as @e[type=creeper,scores={glowTimer=77}] at @s if entity @e[type=creeper,scores={glowTimer=76},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=76}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=77}] at @s unless entity @e[type=creeper,scores={glowTimer=76},distance=..10] run scoreboard players set @s glowTimer 76
execute at @e[type=creeper,scores={glowTimer=77}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=78}] run scoreboard players set @s glowTimer 78
execute as @e[type=creeper,scores={glowTimer=78}] at @s if entity @e[type=creeper,scores={glowTimer=77},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=77}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=78}] at @s unless entity @e[type=creeper,scores={glowTimer=77},distance=..10] run scoreboard players set @s glowTimer 77
execute at @e[type=creeper,scores={glowTimer=78}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=79}] run scoreboard players set @s glowTimer 79
execute as @e[type=creeper,scores={glowTimer=79}] at @s if entity @e[type=creeper,scores={glowTimer=78},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=78}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=79}] at @s unless entity @e[type=creeper,scores={glowTimer=78},distance=..10] run scoreboard players set @s glowTimer 78
execute at @e[type=creeper,scores={glowTimer=79}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=80}] run scoreboard players set @s glowTimer 80
execute as @e[type=creeper,scores={glowTimer=80}] at @s if entity @e[type=creeper,scores={glowTimer=79},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=79}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=80}] at @s unless entity @e[type=creeper,scores={glowTimer=79},distance=..10] run scoreboard players set @s glowTimer 79
execute at @e[type=creeper,scores={glowTimer=80}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=81}] run scoreboard players set @s glowTimer 81
execute as @e[type=creeper,scores={glowTimer=81}] at @s if entity @e[type=creeper,scores={glowTimer=80},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=80}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=81}] at @s unless entity @e[type=creeper,scores={glowTimer=80},distance=..10] run scoreboard players set @s glowTimer 80
execute at @e[type=creeper,scores={glowTimer=81}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=82}] run scoreboard players set @s glowTimer 82
execute as @e[type=creeper,scores={glowTimer=82}] at @s if entity @e[type=creeper,scores={glowTimer=81},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=81}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=82}] at @s unless entity @e[type=creeper,scores={glowTimer=81},distance=..10] run scoreboard players set @s glowTimer 81
execute at @e[type=creeper,scores={glowTimer=82}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=83}] run scoreboard players set @s glowTimer 83
execute as @e[type=creeper,scores={glowTimer=83}] at @s if entity @e[type=creeper,scores={glowTimer=82},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=82}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=83}] at @s unless entity @e[type=creeper,scores={glowTimer=82},distance=..10] run scoreboard players set @s glowTimer 82
execute at @e[type=creeper,scores={glowTimer=83}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=84}] run scoreboard players set @s glowTimer 84
execute as @e[type=creeper,scores={glowTimer=84}] at @s if entity @e[type=creeper,scores={glowTimer=83},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=83}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=84}] at @s unless entity @e[type=creeper,scores={glowTimer=83},distance=..10] run scoreboard players set @s glowTimer 83
execute at @e[type=creeper,scores={glowTimer=84}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=85}] run scoreboard players set @s glowTimer 85
execute as @e[type=creeper,scores={glowTimer=85}] at @s if entity @e[type=creeper,scores={glowTimer=84},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=84}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=85}] at @s unless entity @e[type=creeper,scores={glowTimer=84},distance=..10] run scoreboard players set @s glowTimer 84
execute at @e[type=creeper,scores={glowTimer=85}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=86}] run scoreboard players set @s glowTimer 86
execute as @e[type=creeper,scores={glowTimer=86}] at @s if entity @e[type=creeper,scores={glowTimer=85},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=85}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=86}] at @s unless entity @e[type=creeper,scores={glowTimer=85},distance=..10] run scoreboard players set @s glowTimer 85
execute at @e[type=creeper,scores={glowTimer=86}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=87}] run scoreboard players set @s glowTimer 87
execute as @e[type=creeper,scores={glowTimer=87}] at @s if entity @e[type=creeper,scores={glowTimer=86},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=86}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=87}] at @s unless entity @e[type=creeper,scores={glowTimer=86},distance=..10] run scoreboard players set @s glowTimer 86
execute at @e[type=creeper,scores={glowTimer=87}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=88}] run scoreboard players set @s glowTimer 88
execute as @e[type=creeper,scores={glowTimer=88}] at @s if entity @e[type=creeper,scores={glowTimer=87},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=87}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=88}] at @s unless entity @e[type=creeper,scores={glowTimer=87},distance=..10] run scoreboard players set @s glowTimer 87
execute at @e[type=creeper,scores={glowTimer=88}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=89}] run scoreboard players set @s glowTimer 89
execute as @e[type=creeper,scores={glowTimer=89}] at @s if entity @e[type=creeper,scores={glowTimer=88},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=88}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=89}] at @s unless entity @e[type=creeper,scores={glowTimer=88},distance=..10] run scoreboard players set @s glowTimer 88
execute at @e[type=creeper,scores={glowTimer=89}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=90}] run scoreboard players set @s glowTimer 90
execute as @e[type=creeper,scores={glowTimer=90}] at @s if entity @e[type=creeper,scores={glowTimer=89},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=89}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=90}] at @s unless entity @e[type=creeper,scores={glowTimer=89},distance=..10] run scoreboard players set @s glowTimer 89
execute at @e[type=creeper,scores={glowTimer=90}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=91}] run scoreboard players set @s glowTimer 91
execute as @e[type=creeper,scores={glowTimer=91}] at @s if entity @e[type=creeper,scores={glowTimer=90},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=90}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=91}] at @s unless entity @e[type=creeper,scores={glowTimer=90},distance=..10] run scoreboard players set @s glowTimer 90
execute at @e[type=creeper,scores={glowTimer=91}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=92}] run scoreboard players set @s glowTimer 92
execute as @e[type=creeper,scores={glowTimer=92}] at @s if entity @e[type=creeper,scores={glowTimer=91},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=91}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=92}] at @s unless entity @e[type=creeper,scores={glowTimer=91},distance=..10] run scoreboard players set @s glowTimer 91
execute at @e[type=creeper,scores={glowTimer=92}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=93}] run scoreboard players set @s glowTimer 93
execute as @e[type=creeper,scores={glowTimer=93}] at @s if entity @e[type=creeper,scores={glowTimer=92},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=92}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=93}] at @s unless entity @e[type=creeper,scores={glowTimer=92},distance=..10] run scoreboard players set @s glowTimer 92
execute at @e[type=creeper,scores={glowTimer=93}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=94}] run scoreboard players set @s glowTimer 94
execute as @e[type=creeper,scores={glowTimer=94}] at @s if entity @e[type=creeper,scores={glowTimer=93},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=93}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=94}] at @s unless entity @e[type=creeper,scores={glowTimer=93},distance=..10] run scoreboard players set @s glowTimer 93
execute at @e[type=creeper,scores={glowTimer=94}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=95}] run scoreboard players set @s glowTimer 95
execute as @e[type=creeper,scores={glowTimer=95}] at @s if entity @e[type=creeper,scores={glowTimer=94},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=94}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=95}] at @s unless entity @e[type=creeper,scores={glowTimer=94},distance=..10] run scoreboard players set @s glowTimer 94
execute at @e[type=creeper,scores={glowTimer=95}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=96}] run scoreboard players set @s glowTimer 96
execute as @e[type=creeper,scores={glowTimer=96}] at @s if entity @e[type=creeper,scores={glowTimer=95},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=95}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=96}] at @s unless entity @e[type=creeper,scores={glowTimer=95},distance=..10] run scoreboard players set @s glowTimer 95
execute at @e[type=creeper,scores={glowTimer=96}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=97}] run scoreboard players set @s glowTimer 97
execute as @e[type=creeper,scores={glowTimer=97}] at @s if entity @e[type=creeper,scores={glowTimer=96},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=96}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=97}] at @s unless entity @e[type=creeper,scores={glowTimer=96},distance=..10] run scoreboard players set @s glowTimer 96
execute at @e[type=creeper,scores={glowTimer=97}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=98}] run scoreboard players set @s glowTimer 98
execute as @e[type=creeper,scores={glowTimer=98}] at @s if entity @e[type=creeper,scores={glowTimer=97},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=97}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=98}] at @s unless entity @e[type=creeper,scores={glowTimer=97},distance=..10] run scoreboard players set @s glowTimer 97
execute at @e[type=creeper,scores={glowTimer=98}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=99}] run scoreboard players set @s glowTimer 99
execute as @e[type=creeper,scores={glowTimer=99}] at @s if entity @e[type=creeper,scores={glowTimer=98},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=98}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=99}] at @s unless entity @e[type=creeper,scores={glowTimer=98},distance=..10] run scoreboard players set @s glowTimer 98
execute at @e[type=creeper,scores={glowTimer=99}] as @e[type=creeper,scores={glowTimer=0},distance=..10] unless entity @e[type=creeper,scores={glowTimer=100}] run scoreboard players set @s glowTimer 100
execute as @e[type=creeper,scores={glowTimer=100}] at @s if entity @e[type=creeper,scores={glowTimer=99},distance=1..20] facing entity @e[type=creeper,scores={glowTimer=99}] feet run teleport ^ ^ ^0.25
execute as @e[type=creeper,scores={glowTimer=100}] at @s unless entity @e[type=creeper,scores={glowTimer=99},distance=..10] run scoreboard players set @s glowTimer 99

#Disables AI of any that are not the front
execute as @e[type=creeper,scores={glowTimer=1..}] run data merge entity @s {NoAI:1b}
execute as @e[type=creeper,scores={glowTimer=0}] run data merge entity @s {NoAI:0b}
#Marker/reset
tag @e[type=creeper,scores={glowTimer=1..}] add inconga
scoreboard players set @e[type=creeper,tag=!inconga] glowTimer 0
