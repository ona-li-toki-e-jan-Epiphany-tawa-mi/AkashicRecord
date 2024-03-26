from typing import NoReturn
import PySimpleGUI as sg
from playsound import playsound
from threading import Thread
from pathlib import Path

################################################################################
# MIT License                                                                  #
#                                                                              #
# Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi                        #
#                                                                              #
# Permission is hereby granted, free of charge, to any person obtaining a copy #
# of this software and associated documentation files (the "Software"), to     #
# deal in the Software without restriction, including without limitation the   #
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  #
# sell copies of the Software, and to permit persons to whom the Software is   #
# furnished to do so, subject to the following conditions:                     #
#                                                                              #
# The above copyright notice and this permission notice shall be included in   #
# all copies or substantial portions of the Software.                          #
#                                                                              #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS #
# IN THE SOFTWARE.                                                             #
################################################################################

_moduleDirectory = None

def moduleDirectory() -> str:
    ''' Gets the path of the module directory. '''
    global _moduleDirectory

    if _moduleDirectory is not None:
        return _moduleDirectory

    _moduleDirectory = str(Path(__file__).parent)
    return _moduleDirectory

def runInDaemonThread(function) -> None:
    ''' Launches given function in a separate daemon thread. '''
    thread = Thread(target = function, daemon = True)
    thread.start()



def buildWindow() -> sg.Window:
    ''' Builds the calculator's GUI. '''
    theme = { "yellow": "#FEF200"
            , "blue":   "#3F47CC"
            , "gray":   "#2c2825"
            , "white":  'white'
            , "font":   'Comic Sans MS'
            , "relief": 'sunken'
            }
    flag  = moduleDirectory() + "/images/zastava_bosne_i_hercegovine.png"
    title = "Bosanski Kalkulator"

    keypad = [ ["1",     "2", "3", "+", "del", "clr"]
             , ["4",     "5", "6", "-", "pop", "rll"]
             , ["7",     "8", "9", "*", "dup"]
             , ["(+/-)", "0", "#", "/", "swp"]
             ]

    sg.theme_background_color(theme['gray'])
    sg.theme_button_color((theme['gray'], theme['white']))
    sg.theme_text_color(theme['gray'])
    sg.theme_element_text_color(theme['gray'])

    layout = [ [ sg.Text( size = (80, 10), font = (theme['font'], 15), text_color = theme['yellow'], key = "stack"
                        , relief = theme['relief'], background_color = theme['blue'])]
             , [ sg.Text( size = (80, 1), font = (theme['font'], 15), text_color = theme['blue'], key = "typingArea"
                        , relief = theme['relief'], background_color = theme['yellow'])]
             , [ sg.Col([[sg.Button(button_text = button, font = (theme['font'], 10), size = (3, 2)) for button in row]
                                                                                                     for row in keypad])
               , sg.Col([[sg.Image(flag, subsample = 4)]])
             ] ]

    return sg.Window(title = title, layout = layout, titlebar_font = theme['font'])



class Calculator:
    ''' Represents the state of the calculator. '''
    stack          = []
    typingArea     = []   # Storage for the number being typed in.
    typingAreaSign = True # The selected sign of the number being typed in (positive or negative.)

    def clearTypingArea(self) -> None:
        ''' Clears out the typing area and sets the selected sign to positive, it's default. '''
        self.typingAreaSign = True
        self.typingArea.clear()

def processInput(window: sg.Window, event, values, calculator: Calculator) -> None:
    ''' General input handling method. '''
    if event in "0123456789": # Typing numbers into typing area.
        if event != "0" or calculator.typingArea:
            calculator.typingArea.append(event)
    elif event == "#": # Places number from typing area onto stack.
        if calculator.typingArea:
            sign = 1 if calculator.typingAreaSign else -1
            number = int(''.join(calculator.typingArea)) * sign

            calculator.stack.append(number)
            calculator.clearTypingArea()
    elif event == "+": # Adds top 2 stack values.
        if len(calculator.stack) >= 2:
            calculator.stack.append(calculator.stack.pop() + calculator.stack.pop())
    elif event == "-": # Subtracts top stack value from next top value.
        if len(calculator.stack) >= 2:
            top = calculator.stack.pop()
            calculator.stack.append(calculator.stack.pop() - top)
    elif event == "*": # Multiplies top 2 stack values.
        if len(calculator.stack) >= 2:
            calculator.stack.append(calculator.stack.pop() * calculator.stack.pop())
    elif event == "/": # Divides 2nd from top stack value with top value.
        if len(calculator.stack) >= 2:
            top = calculator.stack.pop()
            calculator.stack.append(calculator.stack.pop() / top)
    elif event == "(+/-)": # Toggles positive/negative mode on typing area.
        calculator.typingAreaSign = not calculator.typingAreaSign
    elif event == "del": # Clears typing area.
        calculator.clearTypingArea()
    elif event == "pop": # Pops top stack value.
        if calculator.stack:
            calculator.stack.pop()
    elif event == "dup": # Duplicates top stack value.
        if calculator.stack:
            calculator.stack.append(calculator.stack[-1])
    elif event == "swp": # Swaps top 2 stack values.
        if len(calculator.stack) >= 2:
            calculator.stack.insert(-1, calculator.stack.pop())
    elif event == "clr": # Clears entire stack.
        calculator.stack.clear()
    elif event == "rll": # Moves top stack value onto the bottom.
        if len(calculator.stack) >= 2:
            calculator.stack.insert(0, calculator.stack.pop())

    # Plays gunfire effect on every button press.
    runInDaemonThread(lambda: playsound(moduleDirectory() + "/audio/pucnjava.mp3"))

def render(window: sg.Window, calculator: Calculator) -> None:
    ''' General render method for updating GUI. '''
    window['stack'].update('\n'.join([str(element) for element in calculator.stack]))
    signString = '' if calculator.typingAreaSign else '-'
    window['typingArea'].update(signString + ''.join(calculator.typingArea))



def loopPlayBosanskaArtiljerija() -> NoReturn:
    ''' Plays Bosanska Artiljerija on repeat. Threadblocking and meant to be used with runInDaemonThread(). '''
    while True:
        playsound(moduleDirectory() + "/audio/bosanska_artiljerija.mp3")



def main() -> None:
    window = buildWindow()
    calculator = Calculator()

    runInDaemonThread(loopPlayBosanskaArtiljerija)

    while True:
        event, values = window.read()

        if event == sg.WIN_CLOSED or event == 'Cancel':
            break

        processInput(window, event, values, calculator)
        render(window, calculator)

    window.close()

if __name__ == '__main__':
    main()
