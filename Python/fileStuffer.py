import os
import os.path as path
import sys


def main():
    ''' Stuffs the contents of one file into another. '''

    source_file = input("What is the path to the source file?: ")

    # Faulty input checking.
    if not path.exists(source_file):
        print ("This path does not exist.")
        input()
        sys.exit()

    elif not path.isfile(source_file):
        print ("This path does not lead to a file.")
        input()
        sys.exit()

    destination_file = input("What is the path to the destination file?: ")

    # Faulty input checking.
    if not path.exists(destination_file):
        print ("This path does not exist.")
        input()
        sys.exit()

    elif not path.isfile(destination_file):
        print ("This path does not lead to a file.")
        input()
        sys.exit()

    source_file = open(source_file, "rb")
    destination_file = open(destination_file, "ab")

    # Meaty boi. Shoves file contents from source into destination.
    destination_file.write(source_file.read())

    source_file.close()
    destination_file.close()
    print("Job done.")
    input()


if __name__ == '__main__':
    main()