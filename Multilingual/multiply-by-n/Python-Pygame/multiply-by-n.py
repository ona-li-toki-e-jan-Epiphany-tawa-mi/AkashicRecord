#!/usr/bin/env python3

import pygame

Seconds    = float
Screen     = pygame.Surface
Frames     = int
Resolution = (int, int)



################################################################################
# Config START                                                                 #
################################################################################
framesPerSecond: Frames = 30

windowSize: Resolution = (400, 400)
################################################################################
# Config END                                                                   #
################################################################################



lastTime = None

def run(screen: Screen, deltaTime: Seconds):
    """ Runs and displays 1 iteration of the animation. """
    screen.fill("black")

    pygame.display.flip() # Display frame.



def main():
    """ This is the Python implementation of muliply-by-n, using Pygame to
        render graphics.  """
    pygame.init()
    screen = pygame.display.set_mode(windowSize)
    clock  = pygame.time.Clock()

    running   = True
    deltaTime = 0.0

    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        run(screen, deltaTime)
        deltaTime = clock.tick(framesPerSecond) / 1000.0

    pygame.quit()

if __name__ == '__main__':
    main()
