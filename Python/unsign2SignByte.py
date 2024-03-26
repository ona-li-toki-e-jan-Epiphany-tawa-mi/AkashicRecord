'''
    Converts an unsigned byte to a signed byte.
    Input is a byte, output is a string.
'''
def sign_byte(unsignedByte):
    byte_signed = bin(unsignedByte - 0b10000000)
    wasNegative = False

    # Deconstructs byte string for bit number correction.
    byte_signed = byte_signed.replace("0b", "", 1)
    if "-" in byte_signed:
        wasNegative = True;
        byte_signed = byte_signed.replace("-", "", 1)

    # Corrects length to 8 bits.
    if len(byte_signed) < 8:
        byte_signed = "0" * (8 - len(byte_signed)) + byte_signed

    elif len(byte_signed) > 8:
        byte_signed = byte_signed[len(byte_signed) - 8:]

    # Reconstructs byte string.
    if wasNegative:
        return "-0b" + byte_signed;

    else:
        return "0b" + byte_signed;
        

while True:
    userInput = input("What byte shall I sign?: ")
    
    if userInput == "none":
        print("Oh, sorry. :(")
        break

    else:
        print( sign_byte(int(userInput, 2)) )
    
