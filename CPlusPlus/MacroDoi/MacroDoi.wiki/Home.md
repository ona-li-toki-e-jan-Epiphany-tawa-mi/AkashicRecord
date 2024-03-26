# MacroDoi
MacroDoi is a macro program that maps user input to actions to be taken by the program (like how pressing CTRL + SHIFT + ESC opens the Task Manager on Windows.)

Macros are loaded from a text file called macros.txt that is in the same directory-level as the executable.

All macros come in the form: 
*activatorName: parameter1; parameter2; ... parameterN | executorName: parameter1; parameter2; ... parameterN*

Activators decide when macros execute, executors decide what happens.

These are the delimiters:
* (\n) Newlines separate macros. 
* | separates activators and executors.
* : separates the name of the component and its parameters.
* ; separates parameters 
* , separates elements in a list.
* \# marks comments.

Macro types:
* [Key Macros](./Key-Macros.md "Key macros wiki page")