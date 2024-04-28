#!/usr/bin/apl --script --

⍝ Returns the names of the files to open specified in the arguments on the
⍝ command line.
∇FILENAMES←GET_FILE_ARGUMENTS
  →(4<≢⎕ARG) / L_HAS_ARGUMENTS
    FILENAMES←⍬ ◊ →L_END_IF
  L_HAS_ARGUMENTS:
    FILENAMES←4↓⎕ARG
  L_END_IF:
∇

⍝ Reads a file with the name ⍵ (string) and returns the lines of that file as a
⍝ vector of strings. Returns ¯2 on missing file.
READ_LINES←{⎕FIO[49] ⍵}



⍝ Opens the file with the given NAME (string) and creates an interactive buffer
⍝ for the user to work with.
∇HANDLE_BUFFER NAME
  ⎕←NAME,":"
  ⎕←READ_LINES NAME
  ⎕←"----------------"
∇

∇MAIN; FILES;BUFFER_NAME
  FILES←GET_FILE_ARGUMENTS

  ⍝ If no files were specified, an empty string is appended to open an empty
  ⍝ buffer to work with.
  →(0≢≢FILES) / L_HAS_FILE
    FILES←⊂""
  L_HAS_FILE:

  ⍝ Iterate through each specified file and open them one-by-one for editing.
  L_FOR_FILES: →(0≡≢FILES) / L_FOR_FILES_END
    BUFFER_NAME←↑FILES
    ⍝ If the dropping results in an empty vector it prints a weird error.
    →(1≡≢FILES) / L_FINAL_FILE
      FILES←1↓FILES ◊ → L_NOT_FINAL_FILE
    L_FINAL_FILE:
      FILES←⍬
    L_NOT_FINAL_FILE:

    ⍝ Do the stuff with the things.
    HANDLE_BUFFER BUFFER_NAME

    →L_FOR_FILES
  L_FOR_FILES_END:
∇
MAIN



)OFF
