import curses
import time
import sys
import os
import secrets
import random

ticks_per_second = 20
max_character_trails = 40
trail_length = range(15, 30)
character_list = "~`!1@2#3$4%5^6&7*8(9)0_-+=QWERTYUIOP{[}]|\\ASDFGHJKL;:'\"ZXCVBNM<,>.?/"
trail_speed = [0.5, 1, 1.5, 2]

# Initialization
window = curses.initscr()
curses.curs_set(0)
window.nodelay(True)

# Does what it says on the tin.
max_y, max_x = window.getmaxyx()
shooting_stars = [] # The cool character things that go down.

def run_program(tikks):
	window.erase()
	
	# Exits program on keypress.
	if window.getch() != -1:
		curses.endwin()
		if sys.platform == 'linux':
			os.system('reset')
		sys.exit()
		
	# Creates trails.
	if tikks % 3 == 0:
		if len(shooting_stars) < max_character_trails:
			shooting_stars.append([random.randint(0, max_x - 1), 0, secrets.choice(trail_speed), []])
			
			# Creates characters.
			for i in range(0, secrets.choice(trail_length)):
				shooting_stars[-1][3].append(secrets.choice(character_list))
				
	# Draws characters and handles trails.
	for star in shooting_stars:
		# Draws characters in valid positions.
		for i in range(0, len(star[3]) - 1):
			if 0 <= int(star[1]) - i < max_y and not (star[0] == max_x - 1 and int(star[1]) - i == max_y - 1):
				window.addch(int(star[1]) - i, star[0], star[3][i])
				
		if tikks % 2 == 0:
			# Moves trails along.
			star[1] += star[2]
			
			# Removes old trails.
			if int(star[1]) - len(star[3]) + 1 >= max_y:
				shooting_stars.remove(star)
				
	window.refresh()
	
# Keeps track if time n' stuff.
tick_time = 1 / ticks_per_second
total_ticks = 0

while True:
	run_program(total_ticks)
	
	time.sleep(tick_time)
	total_ticks += 1