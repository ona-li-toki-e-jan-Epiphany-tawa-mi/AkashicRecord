import winsound
import time

playSound = True
frequency = 2500  # Hz
shortDuration = 100  # ms
longDuration = 400  # ms
breakTime = .05  # ms


alphabet = {'a': ".-", 'b': "-...", 'c': "-.-.", 'd': "-..", 'e': ".", 'f': "..-.", 'g': "--.", 'h': "....", 'i': "..",
            'j': ".---", 'k': "-.-", 'l': ".-..", 'm': "--", 'n': "-.", 'o': "---", 'p': ".--.", 'q': "--.-",
            'r': ".-.", 's': "...", 't': "-", 'u': "..-", 'v': "...-", 'w': ".--", 'x': "-..-", 'y': "-.--",
            'z': "--..", '0': "-----", '1': ".----", '2': "..---", '3': "...--", '4': "....-", '5': ".....",
            '6': "-....", '7': "--...", '8': "---..", '9': "----."}

message = input("Input message: ").lower()

morse = ""
unknown = ""
for x in message: 
    if x == " ":
        morse += "  "
    elif alphabet.keys().__contains__(x):
        morse += alphabet[x] + " "
    else:
        unknown += x

print("\n Input in Morse code: " + morse)
print("\n Nonconvertible characters: " + unknown)


if playSound:
    for x in morse:
        if x == ".":
            winsound.Beep(frequency, shortDuration)
            time.sleep(breakTime)
        elif x == "-":
            winsound.Beep(frequency, shortDuration)
            time.sleep(breakTime * (longDuration / shortDuration))
        elif x == " ":
            time.sleep(breakTime)
