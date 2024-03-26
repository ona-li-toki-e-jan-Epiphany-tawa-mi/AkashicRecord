# %% [markdown]
# # RSA Encryption Assignment.
# 
# This is a script I wrote to help me with doing an assigment where I needed to encode my messages with RSA and decode my classmate's messages

# %%
import string

# First, to generate mappings between the letters and numbers from 1-26, which 
# can be nicely squeezed into one dict.
rsaLetterNumberMap = {}

i = 1
for character in string.ascii_letters[:26]:
    rsaLetterNumberMap[character] = i
    rsaLetterNumberMap[i] = character
    i += 1

# Additional characters.
rsaLetterNumberMap[' '] = 32
rsaLetterNumberMap[32] = ' '
rsaLetterNumberMap['.'] = 27
rsaLetterNumberMap[27] = '.'

print(rsaLetterNumberMap)


# %%
# Construction of the public and private keys.
p = 3 # given.
q = 11 # given.

phi = (p-1)*(q-1)

# Public.
N = p*q
e = 3 # given.

# Private
d = pow(e, -1, phi)

print(f'Public key: {{{N}, {e}}}')
print(f'Private key: {{{d}}}')

# %%
def encrypt(message: string) -> string:
    ''' Takes the given message and encrypts it using the public key we previously created. '''
    encryptedMessage = []

    for character in message.lower():
        number = rsaLetterNumberMap.get(character)
        if number is not None:
            encryptedMessage.append(number)

    # Applies public key.
    for i in range(len(encryptedMessage)):
        encryptedMessage[i] = pow(encryptedMessage[i], e, N)
    
    # Single-digit numbers have a 0 pre-pended to them as required by RSA.
    return ''.join([f'{number:02}' for number in encryptedMessage])

# Given example for testing
assert(encrypt("a cab") == "0132270108")

# %%
import re

notDigitRegex = re.compile('[^0-9]+')

def decrypt(encryptedMessage: string) -> string:
    ''' Takes the given encrypted message and decrypts it using the private key we previously created.'''
    encryptedMessage = re.sub(notDigitRegex, '', encryptedMessage)
    if len(encryptedMessage) % 2 != 0:
        raise ValueError("An encrypted message needs two number per character, as-in a string of even length, to be valid")
    
    message = []

    # Decrypts the message with the private key.
    for i in range(0, len(encryptedMessage), 2):
        message.append(pow(int(encryptedMessage[i:i+2]), d, N))

    return ''.join(filter(lambda x: x is not None, [rsaLetterNumberMap.get(number) for number in message]))

assert(decrypt("0132270108") == "a cab")

# %%
# My submission for the assignment.
# Note: '.' is 27.

postTitle = encrypt("Discussion: Cryptography")
print(f'Post title: {postTitle}')
print(f'Decrypted: {decrypt(postTitle)}')

initialPost = encrypt("The most interesting thing in this course would have to be this assignment. Using something you learned to do something cool is something that is not done anywhere near enough in the classes I have taken. to make this assignment easier for me I wrote some Python code to do the work, which you can find a Jupyter notebook attached to this post if you would like to use it yourself. I noticed that I couldn't decrypt others' messages, however my code correctly encrypts and decrypts the 'A CAB' message that was supplied to test our work, so I'm led to believe that they are incorrectly done. To calculate the value of d I used Python's pow function. You can compute an inverse multiple by putting the number to invert first, which is e, then negative one, and you can add in number to mod that by with the third argument, which is phi. You can use separate operators but pow is optimized for integers, which is cool.")

print()
print(f'Initial Post: {initialPost}')
print(f'Decrypted: {decrypt(initialPost)}')


