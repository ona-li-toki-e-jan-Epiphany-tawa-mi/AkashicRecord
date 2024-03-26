import pyvjoy
import time

""" Creates 3 extra players in the local lobby of Stick Fight: The Game for achievement hunting.
Usage: Run script, open the game window, and wait.

VJoy and the pyvjoy library need to be installed, and the controllers 1, 2, and 3 need to be added and configured."""

fakePlayers = [pyvjoy.VJoyDevice(1), pyvjoy.VJoyDevice(2), pyvjoy.VJoyDevice(3)]

# Delay so the user can enter the game before the script starts.
time.sleep(10)

# Random button press to create players.
for player in fakePlayers:
    player.set_button(1, 1)

time.sleep(0.2)

for player in fakePlayers:
    player.set_button(1, 0)
