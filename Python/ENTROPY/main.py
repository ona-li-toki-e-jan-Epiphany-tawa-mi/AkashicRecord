import tkinter as tk  # totally needed
from tkinter import messagebox  # Absolutely necessary
import pygame  # Kinda need this
import time  # Used for tracking time

root = tk.Tk()  # Gets rid of the ugly
root.withdraw()  # Gets rid of the ugly

pygame.init()  # If only the robot was this simple, god forbid actually having to type more than one line of code

# D-d-d-default resolution
dispX = 1280
dispY = 720

# Colors
black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)
blue = (0, 0, 255)

# logo = pygame.image.load("logo32x32.png") # Loads an icon for the game window
# pygame.display.set_icon(logo) # Displays an icon for the game window
pygame.display.set_caption("ENTROPY")  # A good game in the name of Gracious Professionalism
screen = pygame.display.set_mode((dispX, dispY))  # Starts with a specific resolution

# Some loaded images
oseq1 = pygame.transform.scale(pygame.image.load('Images/OpeningSeq/pylogon.png'), (dispX, dispY))

clock = pygame.time.Clock()  # Tick, tick, tick, lick ticks...

running = True  # How can you run if you have no legs?

while running:  # Ummm... t-t-that's a genuine question... y'know?
    timer = time.perf_counter()  # Measures time
    if 3 <= timer <= 6:  # Just like the bigger games!
        screen.blit(oseq1, (0, 0))  # I love it
    else:
        screen.fill(black)  # Resets screen

    for event in pygame.event.get():  # Spits out game events
        if event.type == pygame.QUIT:  # Only runs for quitters
            # Better back out now Man!
            if messagebox.askyesno("Okay, this is unebic!", "Are you sure you want to quit?"):
                running = False  # Long live the king

    pygame.display.update()  # hey
    clock.tick(60)  # Get rekt fze



