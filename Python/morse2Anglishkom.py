malphabet = {".-": 'a', "-...": 'b', "-.-.": 'c', "-..": 'd', '.': 'e', "..-.": 'f', "--.": 'g', "....": 'h', "..": 'i',
            ".---": 'j', "-.-": 'k', ".-..": 'l', "--": 'm', "-.": 'n', "---": 'o', ".--.": 'p', "--.-": 'q',
            ".-.": 'r', "...": 's', '-': 't', "..-": 'u', "...-": 'v', ".--": 'w', "-..-": 'x', "-.--": 'y',
            "--..": 'z', "-----": '0', ".----": '1', "..---": '2', "...--": '3', "....-": '4', ".....": '5',
            "-....": '6', "--...": '7', "---..": '8', "----.": '9', "-.-.--": '!', '|': ' '}
ealphabet = {}

for mor, eng in malphabet.items():
    ealphabet[eng] = mor
      

def morse2english(morse):
    if morse == "":
        return ""
    
    english = ""
    tenp = morse
    
    tenp = tenp.replace('_', '-')
    tenp = tenp.replace('/', '|')

    for i in tenp.split(' '):
        if malphabet.__contains__(i):
            english += malphabet[i]

    return english
    
    
def english2morse(english):
    if english == "":
        return ""
    
    morse = ""
    tenp = english.lower()
    
    for i in tenp:
        if ealphabet.__contains__(i):
            morse += ealphabet[i]
            morse += ' '
            
    return morse        
    
print('\n')
    
nod = "this is a cool python program!"

print(nod)

nod = english2morse(nod)

print(nod)

nod = morse2english(nod)

print(nod)