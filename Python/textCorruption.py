import random
import string

'''
A small program I whipped up to generate text that corrupts over time, like it's melting.
'''

# No input protection because haha funny funny.
text = list(input("Enter some text to corrupt: "))
times = int(input("Enter the number of times to corrupt text: "))
corruption_rate = float(input("Enter the rate of curruption (between 0 and 1): "))
print()


characterSet = string.ascii_letters + string.digits

print(str(''.join(text)))
for i in range(1, times + 1):
    # Replaces characters randomly.
    for k in range(0, len(text)):
        if random.random() < corruption_rate:
            text[k] = random.choice(characterSet)
    
    print(str(''.join(text)))

    # Increases the corruption rate "logarithmically."
    corruption_rate = min(corruption_rate + corruption_rate / (i * i), 1)


print()
input("Press ENTER to exit.")
