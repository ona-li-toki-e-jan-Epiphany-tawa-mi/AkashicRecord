# implement AES-256 for encryption and decryption

import sys
import argparse
import os.path
import secrets
import string

AES_s_box = [0] * 256


def ROTL8(x, shift):
    ''' I'm guessing it rotates bits. I just copied it from wikipedia lol. https://en.wikipedia.org/wiki/Rijndael_S-box '''
    return (x << shift) | (x >> (8 - (shift)))


def intialize_s_box():
    ''' Intializes the substitution box. I just copied it from wikipedia lol. https://en.wikipedia.org/wiki/Rijndael_S-box '''
    p, q = 1, 1

    while True:
        p = p ^ (p << 1) ^ (0x1B if p & 0x80 != 0 else 0)

        q ^= q << 1
        q ^= q << 2
        q ^= q << 4
        q ^= 0x09 if q & 0x80 != 0 else 0

        xformed = q ^ ROTL8(q, 1) ^ ROTL8(q, 2) ^ ROTL8(q, 3) ^ ROTL8(q, 4)

        AES_s_box[p] = xformed ^ 0x63

        if p != -1:
            break;

    AES_s_box[0] = 0x63

    print(len(AES_s_box))
    

def sub_bytes(byte_array):
    for i in range(0, 32):
        byte_array[i] = AES_s_box[byte_array[i]]


def xor_key(byte_array, key):
    for i in range(0, 32):
        byte_array[i] ^= (key >> ((31 - i) << 3)) & 0xFF
        

def encrypt_AES256(byte_string, key):
    byte_array = [byte for byte in byte_string]

    xor_key(byte_array, key)    
        
    for i in range(0, 13):
        sub_bytes(byte_array)
    
    return bytes(byte_array)


def decrypt_AES256(bytes_tring, key):
    byte_array = int.from_bytes(byte_string, byteorder = 'big')
    return byte_array.to_bytes(32, byteorder = 'big')


def key_to_int(key):
    value = 0

    for i in range(0, 32):
        value += ord(key[i]) << (i << 3)

    return value


parser = argparse.ArgumentParser(description = "Encrypts or decrypts files with AES-256.")
parser.add_argument("mode", choices = ["e", "encrypt", "d", "decrypt"], help = "Whether to encrypt or decrypt the file.", metavar = "Mode (e, encrypt, d, decrypt)")
parser.add_argument("inputFile", help = "The file to read from.", metavar = "InputFile")
parser.add_argument("-o", "--outputFile", help = "The file to output the result to, defaults to input file.")
parser.add_argument("-k", "--key", help = "The 32-byte key used to encrypt or decrypt the data. Will create one if not specified and encrypting.")

arguments = parser.parse_args()
input_file = 0

try:
    input_file = open(arguments.inputFile, "rb")

except OSError as exception:
    sys.exit(exception)

output_file = 0

# Gets output file.
if arguments.outputFile is not None:
    try:
        output_file = open(arguments.outputFile, "wb")

    except OSError as exception:
        sys.exit(exception)

else:
    try:
        if arguments.mode == "e" or arguments.mode == "encrypt":
            output_file = open("encrypted-" + arguments.inputFile, "wb")
            
        else:
            output_file = open("decrypted-" + arguments.inputFile, "wb")

    except OSError as exception:
        sys.exit(exception)

read_data = input_file.read(32)
AES_function = encrypt_AES256 if arguments.mode == "e" or arguments.mode == "encrypt" else decrypt_AES256
key = arguments.key

if key is None:
    if arguments.mode == "e" or arguments.mode == "encrypt":
        key_set = string.ascii_letters + string.digits + string.punctuation
        key = ""
    
        for i in range(0, 32):
            key += secrets.choice(key_set)
    
        print("Generated key: " + key)

    else:
        sys.exit("")

elif len(key) != 32:
    sys.exit("Error: The given key is not 32 characters long")

key = key_to_int(key)
intialize_s_box()

while read_data != b'':
    # Ensures all data is 256 bits long.
    data_length = len(read_data)
    if data_length < 32:
        read_data += secrets.token_bytes(32 - data_length)

    output_file.write(AES_function(read_data, key))
    
    read_data = input_file.read(32)
    

print("Finished.")
input_file.close()
output_file.close()
