# Key Macros
Key macros interact with the keyboard, looking for key-presses and executing synthetic ones.

Both the activator and executor are named Key.

## Key activators
Key activators take a list of keys and a reaction time. The keys decide which keys need to be pressed to activate the macro. The reaction time is the amount of time, in seconds, given between each key-press before the macro resets and the user will have to start again. Longer macros require more time.

Their structure is like this: *Key: key1, key2, ... keyN; reactionTime*

There must be at least one key. The reaction time must be greater than 0.

### Allowed values:
For keys, the following list contains the allowed values (case-sensitive.)

[Look here](https://docs.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes "Windows Virtual Key Codes") and [here](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values "Key Values of the Web") for more information.

| Value | Description |
|-|-|
| LEFT_BUTTON | The left mouse button. |
| RIGHT_BUTTON | The right mouse button. |
| CANCEL | Control-break processing.<br>Usually CTRL + C. |
| MIDDLE_BUTTON | The middle mouse button. |
| XBUTTON1 | The X1 mouse button on 5-button mouses. |
| XBUTTON2 | The X2 mouse button on 5-button mouses. |
| BACKSPACE | The BACKSPACE key. |
| TAB | The TAB key. |
| CLEAR | The CLEAR key.<br>Like the 5 key on the keypad when number lock is off. |
| ENTER | The ENTER key. |
| SHIFT | The SHIFT key. |
| CONTROL | The CONTROL key. |
| ALT | The ALT key. |
| PAUSE | The PAUSE key.<br>Pauses the current application or state, if applicable. |
| CAPS_LOCK | The CAPS LOCK key. |
| KANA | IME Kana mode. |
| HANGUL | IME Hangul mode. |
| JUNJA | IME Junja mode. |
| FINAL | IME final mode. (?) |
| HANJA | IME Hanja mode. |
| KANJI | IME Kanji mode. |
| ESCAPE | The ESCAPE key. |
| CONVERT | The IME convert key.<br>Tells the IME to convert the current input sequence into a character. |
| NONCONVERT | The IME nonconvert key.<br>Accepts the current input sequence in the IME without converting it. |
| ACCEPT | The IME accept key.<br>Accepts the selected option or input conversion method. |
| MODECHANGE | The IME mode change key.<br>Cycles through IME input modes. |
| SPACE | The Spacebar. |
| PAGE_UP | The PAGE UP key. |
| PAGE_DOWN | The PAGE DOWN key. |
| END | The END key. |
| HOME | The HOME key. |
| LEFT_ARROW | The LEFT ARROW key. |
| UP_ARROW | The UP ARROW key. |
| RIGHT_ARROW | The RIGHT ARROW key. |
| DOWN_ARROW | The DOWN ARROW key. |
| SELECT | The SELECT key. (?) |
| PRINT | The PRINT key. |
| EXECUTE | The EXECUTE key. (?) |
| PRINT_SCREEN | The PRINT SCREEN key. |
| INSERT | The INSERT key. |
| DELETE | The DELETE key. |
| HELP | The HELP key.<br>Displays help information. |
| 0 | The 0 key. |
| 1 | The 1 key. |
| 2 | The 2 key. |
| 3 | The 3 key. |
| 4 | The 4 key. |
| 5 | The 5 key. |
| 6 | The 6 key. |
| 7 | The 7 key. |
| 8 | The 8 key. |
| 9 | The 9 key. |
| A | The A key. |
| B | The B key. |
| C | The C key. |
| D | The D key. |
| E | The E key. |
| F | The F key. |
| G | The G key. |
| H | The H key. |
| I | The I key. |
| J | The J key. |
| K | The K key. |
| L | The L key. |
| M | The M key. |
| N | The N key. |
| O | The O key. |
| P | The P key. |
| Q | The Q key. |
| R | The R key. |
| S | The S key. |
| T | The T key. |
| U | The U key. |
| V | The V key. |
| W | The W key. |
| X | The X key. |
| Y | The Y key. |
| Z | The Z key. |
| LEFT_WINDOWS | The Left Windows key. |
| RIGHT_WINDOWS | The Right Windows key. |
| APPS | The Applications key.<br>Opens the context menu, like right-clicking. |
| SLEEP | The Computer Sleep key. |
| NUMPAD_0 | The 0 key on the keypad. |
| NUMPAD_1 | The 1 key on the keypad. |
| NUMPAD_2 | The 2 key on the keypad. |
| NUMPAD_3 | The 3 key on the keypad. |
| NUMPAD_4 | The 4 key on the keypad. |
| NUMPAD_5 | The 5 key on the keypad. |
| NUMPAD_6 | The 6 key on the keypad. |
| NUMPAD_7 | The 7 key on the keypad. |
| NUMPAD_8 | The 8 key on the keypad. |
| NUMPAD_9 | The 9 key on the keypad. |
| MULTIPLY | The '*' key on the keypad. |
| ADD | The '+' key on the keypad. |
| SEPARATOR | The Separator key.<br>For the US standard keyboard, a comma; for other keyboards it is frequently a period. |
| SUBTRACT | The '-' key on the keypad. |
| DECIMAL | The Decimal key.<br>For the US standard keyboard, a period; for other keyboards it is frequently a comma. | 
| DIVIDE | The '/' key on the keypad. |
| F1 | The F1 key. |
| F2 | The F2 key. |
| F3 | The F3 key. |
| F4 | The F4 key. |
| F5 | The F5 key. |
| F6 | The F6 key. |
| F7 | The F7 key. |
| F8 | The F8 key. |
| F9 | The F9 key. |
| F10 | The F10 key. |
| F11 | The F11 key. |
| F12 | The F12 key. |
| F13 | The F13 key. |
| F14 | The F14 key. |
| F15 | The F15 key. |
| F16 | The F16 key. |
| F17 | The F17 key. |
| F18 | The F18 key. |
| F19 | The F19 key. |
| F20 | The F20 key. |
| F21 | The F21 key. |
| F22 | The F22 key. |
| F23 | The F23 key. |
| F24 | The F24 key. |
| NUMBER_LOCK | The NUMBER LOCK key. |
| SCROLL_LOCK | The SCROLL LOCK key. |
| LEFT_SHIFT | The Left SHIFT key. |
| RIGHT_SHIFT | The Right SHIFT key. |
| LEFT_CONTROL | The Left CONTROL key. |
| RIGHT_CONTROL | The Right CONTROL key. |
| LEFT_ALT | The Left MENU key. |
| RIGHT_ALT | The Right MENU key. |
| BROWSER_BACK | The Browser Back key.<br>Goes to the previous page. |
| BROWSER_FORWARD | The Browser Forward key.<br>Goes to the next page. |
| BROWSER_REFRESH | The Browser Refresh key. |
| BROWSER_STOP | The Browser Stop key.<br>Stops loading the current page. |
| BROWSER_SEARCH | The Browser Search key.<br>Activates a search bar or the preferred search engine in the browser. |
| BROWSER_FAVORITES | The Browser Favorites key.<br>Opens the user's bookmarks/favorites. |
| BROWSER_HOME | The Browser Home key.<br>Opens the user's preferred homepage. |
| VOLUME_MUTE | The Volume Mute key. |
| VOLUME_DOWN | The Volume Down key. |
| VOLUME_UP | The Volume Up key. |
| NEXT_TRACK | The Next Track key. |
| PREVIOUS_TRACK | The Previous Track key. |
| MEDIA_STOP | The Stop Media key. |
| PLAY_PAUSE | The Play/Pause Media key. |
| LAUNCH_MAIL | The Start Mail key.<br>Launches the preferred mailing application. |
| MEDIA_SELECT | The Select Media key.<br>Launches the preferred media player. |
| LAUNCH_APP1 | The Start Application 1 key.<br>Launches whatever application is bound to this key. |
| LAUNCH_APP2 | The Start Application 2 key.<br>Launches whatever application is bound to this key. |
| OEM_1 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the ';:' key. |
| PLUS | For any country/region, the '+' key. |
| COMMA | For any country/region, the ',' key. |
| MINUS | For any country/region, the '-' key. |
| PERIOD | For any country/region, the '.' key. |
| OEM_2 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the '/?' key. |
| OEM_3 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the '`~' key. |
| OEM_4 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the '[{' key. |
| OEM_5 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the '\|' key. |
| OEM_6 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the ']}' key. |
| OEM_7 | Used for miscellaneous characters; it can vary by keyboard.<br>For the US standard keyboard, the 'single quote/double quote' key. |
| OEM_8 | Used for miscellaneous characters; it can vary by keyboard. |
| OEM_102 | Either the angle bracket key or the backslash key on the RT 102-key keyboard. |
| PROCESS | The IME PROCESS key.<br>Tells the IME to process the conversion. |
| ATTENTION | The Attention key.<br>Refreshes the screen. |
| CONTROL_SELECT | The Cursor Select key. (?) |
| EXTENDED_SELECT | The Extended Selection key. (?) |
| ERASE_EOF | The Erase EOF key.<br>Deletes all characters from the current cursor position to the end of the field. |
| PLAY | The Play key.<br>Resumes paused applications. |
| ZOOM | The Zoom key.<br>Toggles zooming. |
| PROGRAM_ACTION1 | The Program Action 1 key.<br>Ends the current task. |
| OEM_CLEAR | The Clear key.<br>Clears the currently selected input. |

## Key executors
Key executors take a list of keys. The keys decide which keys are to be pressed when the macro activates. You can also put a time to wait, in milliseconds, by writing WAIT + time (e.x. WAIT400 which waits for 0.4 seconds.) This will pause the key presses. Useful if things need time before typing can continue, like opening a command prompt and typing a command.

Their structure is like this: *Key: key1, key2, ... keyN*

There must be at least one key. WAIT instructions must have an integer value directly after their name.

### Allowed values:
The same as key activators plus WAIT instructions.

## Examples:
\# Replaces hello with Goodbye.

Key: H, E, L, L, O; 0.4 | Key: BACKSPACE, BACKSPACE, BACKSPACE, BACKSPACE, BACKSPACE, SHIFT, G, O, O, D, B, Y, E

\# Opens Paint when paint is typed.

Key: P, A, I, N, T; 0.4 | Key: RIGHT_WINDOWS, WAIT200, P, A, I, N, T, WAIT200, ENTER

\# Shuts down the computer when z, x, and c are pressed.

Key: Z, X, C; 0.1 | Key: RIGHT_WINDOWS, WAIT200, C, M, D, WAIT200, ENTER, WAIT600, S, H, U, T, D, O, W, N, SPACE, OEM_2, S, SPACE, OEM_2, F, SPACE, OEM_2, T, SPACE, 0, ENTER