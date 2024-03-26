#!/usr/bin/env python3

rooms = {
        'Great Hall': {'South': 'Bedroom'},
        'Bedroom': {'North': 'Great Hall', 'East': 'Cellar'},
        'Cellar': {'West': 'Bedroom'} }



currentRoom = 'Bedroom' # Need to wake up sometime y'know.

while True:
    print(f"You are currently in the {currentRoom}.")

    for direction, ajoinedRoom in rooms[currentRoom].items():
        print(f"There is a {ajoinedRoom} to the {direction}.")

    print()
    # The list comprehension here decays the key list into a normal list so that it prints nicely.
    userChoice = input(f"In what direction would you like to go {[direction for direction in rooms[currentRoom].keys()]}? (type 'exit' to leave the game.): ")

    print('\n\n\n', end='') # Some spacing to make message prompts prettier and easier to read.


    if userChoice == 'exit': # NOTE: Would be better to just break here.
        currentRoom = 'exit'

    else:
        try:
            currentRoom = rooms[currentRoom][userChoice]

        except KeyError:
            print(f"Unknown command '{userChoice}'")

    if currentRoom == 'exit':
        break



print("Goodbye...")
