from datetime import date

név = input('What is your name? ')
életkor = int(input('How old are you? '))

születésiÉv = date.today().year - életkor

print()
print(f'Hello {név}! You were born in {születésiÉv}.')