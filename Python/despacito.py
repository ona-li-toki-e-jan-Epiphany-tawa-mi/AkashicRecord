alphC = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]  # Usable character count holder
alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u",
            "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
            "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", " ", "~", "`", "!", "1", "@", "2", "#", "3", "$", "4",
            "%", "5", "^", "6", "&", "7", "*", "8", "(", "9", ")", "0", "_", "-", "+", "=", "{", "[", "}", "]", "|",
            "\\", ":", ";", "\"", "'", "<", ",", ">", ".", "?", "/"]  # The alphabet, and various other characters

word = input("Say the word: ")  # Accepts a word from user input
target = input("Into what tword?: ")  # Accepts a target word from user input
output = ""  # Stores word that was stringed together

for x in word:  # Splits the word input character by character
    z = ""  # Used to stop characters being read from the same position multiple times
    for y in target:  # Splits the target word input character by character
        if x == y and y != z:  # Compares characters
            alphC[alphabet.index(x)] += 1  # Collects together usable characters
            z = y  # Stops characters from being read from the same position multiple times

for x in alphC:  # Scans every character count in the array
    # Scans for usable characters and capitals
    if alphC[alphC.index(x)] > 0 and 25 < alphC.index(x) <= 51:
        # Prints out the number of capital usable characters
        print(str(alphC[alphC.index(x)])+", "+str(alphabet[alphC.index(x)])+" -c")
    elif alphC[alphC.index(x)] > 0:  # Scans for usable characters
        # Prints out the number of usable characters
        print(str(alphC[alphC.index(x)]) + ", " + str(alphabet[alphC.index(x)]))

attR = len(word)  # Makes the run cycles the number of characters in the word input
cntr = 0  # Counts how many full words have been constructed

while attR >= 0:  # Regulates runs cycles based on character count of input word
    for x in target:  # Checks every character in the target word
        if alphC[alphabet.index(x)] > 0:  # Builds words if usable characters are available
            output += x  # Strings together words to form characters
            alphC[alphabet.index(x)] -= 1  # Removes characters to simulate usage
    if output == target:  # Function that cycles through the built words
        print(output)  # Prints out full words
        output = ""  # Resets output to simulate usage
        cntr += 1  # Counts up fully constructed words
    attR -= 1  # Progresses while loop
print("\nExcess: "+output+"\n")  # Simply prints out excess characters
# Prints out the number of constructed words and leftover usable characters
print(str(cntr)+" word(s) constructed with "+str(len(output))+" leftover usable character(s)")
