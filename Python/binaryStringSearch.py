import argparse
import os
import os.path as path
import time
import mimetypes

# Sets up arguments so the code can be easily executed in the CMD window.
argParser = argparse.ArgumentParser(description="Reads Binaries for sequences of ASCII characters.",
                                    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
argParser.add_argument("file_name", help="The name of the binary executable to read.")
argParser.add_argument("-p", "--path", help="The path to the file, should it be elsewhere.", default=os.getcwd())
argParser.add_argument("-ml", "--minLength", type=int, help="Sets minimum length of strings to search for.", default=5)
argParser.add_argument("-w", "--write", help="Writes output to a file instead of to the console.", action="store_true",
                       default=False)
argParser.add_argument("-ch", "--characters", help="Sets a different array of characters to look for.",
                       default="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ`1234567890-=~!@#$%^&*()_+[]\\{}|;'"
                               ":\",./<>? ")


# A simple exception to show when an incorrect file type is used, this code currently does nothing.
class InvalidFileTypeException(Exception):
    def __init__(self, *args, **kwargs):
        pass


if __name__ == "__main__":
    args = argParser.parse_args()

    if not path.exists(args.path):
        raise NotADirectoryError(args.path + " is not a valid directory")
    if not path.isfile(args.path + '\\' + args.file_name):
        raise FileNotFoundError("No file named '" + args.file_name + "' in directory '" + args.path + "'")

	# Currently does nothing because I'm lazy and don't want to add checks for various binary file types
    # if not args.file_name.split('.')[-1] == "exe":
        # raise InvalidFileTypeException("File '" + args.file_name + "' must an executable")

	# Opens up the file in binary read mode.
    binary = open(args.path + '\\' + args.file_name, mode='rb')
    binaryData = binary.read()

    outputTxtFile = 0
    if args.write:
        outputTxtFile = open("stringSearchOutput.txt", 'a')

	# Various 'pointers' to help keep check of the current state of the program. Sequences of printable characters that
    # are in args.characters build up the string buffer. Once the program encounters a character that is not printable
    # or in the defined character set, the string buffer is either printed to console or written to file should it be as
    # long or longer than args.minLength. After this check, the buffer is purged.
    stringStartPointer = 0
    stringEndPointer = 0
    fileReadPointer = 0
    stringBuffer = ''

    for i in binaryData:
        if 32 <= i <= 127 and chr(i) in args.characters:
            stringEndPointer = fileReadPointer
            stringBuffer += chr(i)
        else:
            if stringEndPointer - stringStartPointer > args.minLength:
                if args.write:
                    outputTxtFile.write(stringBuffer + '\n')
                else:
                    print(stringBuffer)

            stringBuffer = ''
            stringStartPointer = fileReadPointer

        fileReadPointer += 1
