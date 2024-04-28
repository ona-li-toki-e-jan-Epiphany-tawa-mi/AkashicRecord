#!/usr/bin/apl --script --

⍝ Starts the ncurses server and returns it's file HANDLE.
∇HANDLE←NCUR∆SERVER_START
  ⍝ Uses popen() to launch the server in a subprocess.
  HANDLE←"w" ⎕FIO[24] "./ncurses-server"
∇

⍝ Closes the ncurses server with the given HANDLE.
∇NCUR∆SERVER_STOP HANDLE
  ⍝ Uses pclose() to shutdown the server.
  ⊣ ⎕FIO[25] HANDLE
∇

⍝ Sends a TLV message (TAG_VALUE) to the server with the given file HANDLE. The
⍝ message needs to be a ravel of the tag (number corresponding to an ncurses
⍝ command) and the value (string representing the arguments of the command.) An
⍝ empty string is used for no arguments.
∇HANDLE NCUR∆SERVER_SEND TAG_VALUE
   ⍝ Encodes message as TLV and fwrite()s it to the server.
  ⊣ (⎕UCS 33 ⎕CR TAG_VALUE) ⎕FIO[7] HANDLE
  ⍝ Flush output to make sure command is sent immediately.
  ⊣ ⎕FIO[16] HANDLE
∇



∇NCUR∆INITSCR HANDLE
  HANDLE NCUR∆SERVER_SEND 0,""
∇
∇HANDLE NCUR∆PRINTW STRING
  HANDLE NCUR∆SERVER_SEND 1,STRING
∇
∇NCUR∆REFRESH HANDLE
  HANDLE NCUR∆SERVER_SEND 2,""
∇
∇NCUR∆GETCH HANDLE
  HANDLE NCUR∆SERVER_SEND 3,""
∇
∇NCUR∆ENDWIN HANDLE
  HANDLE NCUR∆SERVER_SEND 4,""
∇
∇NCUR∆CLEAR HANDLE
  HANDLE NCUR∆SERVER_SEND 5,""
∇

HANDLE←NCUR∆SERVER_START

NCUR∆INITSCR HANDLE
HANDLE NCUR∆PRINTW "Hello, world!"
NCUR∆REFRESH HANDLE
NCUR∆GETCH HANDLE
NCUR∆ENDWIN HANDLE

NCUR∆SERVER_STOP HANDLE

)OFF
