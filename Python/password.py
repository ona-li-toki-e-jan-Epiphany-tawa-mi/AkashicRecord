import secrets
import string


def main():
    ''' Generates a password of the length specified by the user. '''

    password_length = input("How many characters long should the password be?: ")

    if password_length.isdecimal():
        password_length = int(password_length)

        # Generates password.
        if password_length > 0:
            character_set = string.ascii_letters + string.digits
            password = ""

            for i in range(password_length):
                password = password + secrets.choice(character_set)

            print(password)

        else:
            print("Invalid length.")

    else:
        print("Invalid length.")

    input()


if __name__ == '__main__':
    main()