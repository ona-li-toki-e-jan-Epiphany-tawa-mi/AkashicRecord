/*
 * This file is part of Brainblast-Toolkit.
 *
 * Brainblast-Toolkit is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Brainblast-Toolkit is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Brainblast-Toolkit. If not, see <https://www.gnu.org/licenses/>.
 */

/*
 * BASICfuck Read-Evaluate-Print Loop.
 *
 * Requires BASICFUCK_MEMORY_SIZE to be defined to be the number of BASICfuck
 * cells to allocate for the REPL to use.
 */

#include <conio.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include <unistd.h>
#include <assert.h>

#include "text_buffer.h"
#include "bytecode_compiler.h"
#include "opcodes.h"
#include "keyboard.h"
#include "screen.h"




// The size of the memory for the compiled bytecode of entered BASICfuck code.
#define PROGRAM_MEMORY_SIZE 256U
// Memory for the compiled bytecode of entered BASICfuck code.
Opcode program_memory[PROGRAM_MEMORY_SIZE];

// Global variables for sharing information with the interpreter.
uchar* BASICfuck_memory;                          // BASICfuck cell memory.
uint   BASICfuck_memory_index;                    // the current index into cell memory.
uchar* computer_memory_pointer;                   // the current index into raw computer memory.



// Global variables for exchaning values with inline assembler. TODO
//uchar RegisterA, RegisterX, RegisterY;
//uint  JumpAddress;

// TODO make sure this actually works. (jsr may be jumping to variable location rather than value.)
/**
 * Runs the BASICfuck execute instruction. TODO
 */
void execute() {
    /*
    RegisterA   = BASICfuck_memory[BASICfuck_memory_index];
    RegisterX   = BASICfuck_memory[BASICfuck_memory_index+1];
    RegisterY   = BASICfuck_memory[BASICfuck_memory_index+2];
    JumpAddress = (uint)computer_memory_pointer;

    __asm__ volatile ("lda     %v", RegisterA);
    __asm__ volatile ("ldx     %v", RegisterX);
    __asm__ volatile ("ldy     %v", RegisterY);
    __asm__ volatile ("jsr     %v", JumpAddress);
    __asm__ volatile ("sta     %v", RegisterA);
    __asm__ volatile ("stx     %v", RegisterX);
    __asm__ volatile ("sty     %v", RegisterY);

    BASICfuck_memory[BASICfuck_memory_index]   = RegisterA;
    BASICfuck_memory[BASICfuck_memory_index+1] = RegisterX;
    BASICfuck_memory[BASICfuck_memory_index+2] = RegisterY;
    */
}

/**
 * Runs the interpreter with the given bytecode-compiled BASICfuck program,
 * leaving the given starting state off wherever it the program finished at.
 *
 * @param program_memory (global) - the BASICfuck program. Must be no larger
 *        than 256 bytes.
 * @param BASICfuck_memory (global) - the starting address of BASICfuck memory.
 * @param BASICfuck_memory_index (global) - the current index into BASICfuck
 *        memory.
 * @param computer_memory_pointer (global) - the current index into raw computer
 *        memory.
 */
void run_interpreter() {
    Opcode opcode;
    uchar  argument;

    uchar program_index = 0;

    static const void *const jump_table[] = {
        &&lopcode_end_program,                    // BASICFUCK_END_PROGRAM.
        &&lopcode_increment,                      // BASICFUCK_INCREMENT.
        &&lopcode_decrement,                      // BAISCFUCK_DECREMENT.
        &&lopcode_bfmemory_left,                  // BASICFUCK_BFMEMORY_LEFT.
        &&lopcode_bfmemory_right,                 // BASICFUCK_BFMEMORY_RIGHT.
        &&lopcode_print,                          // BASICFUCK_PRINT.
        &&lopcode_input,                          // BASICFUCK_INPUT.
        &&lopcode_jeq,                            // BASICFUCK_JEQ.
        &&lopcode_jne,                            // BASICFUCK_JNE.
        &&lopcode_cmemory_read,                   // BASICFUCK_CMEMORY_READ.
        &&lopcode_cmemory_write,                  // BASICFUCK_CMEMORY_WRITE.
        &&lopcode_cmemory_left,                   // BASICFUCK_CMEMORY_LEFT.
        &&lopcode_cmemory_right,                  // BASICFUCK_CMEMORY_RIGHT.
        &&lopcode_execute                         // BASICFUCK_EXECUTE.
    };


    while (true) {
        if (kbhit() != 0 && cgetc() == KEYBOARD_STOP) {
            puts("?ABORT");
            break;
        }


        opcode   = program_memory[program_index];
        argument = program_memory[program_index+1];
        assert(opcode < sizeof(jump_table)/sizeof(jump_table[0]));
        goto *jump_table[opcode];

    lopcode_end_program:
        break;

    lopcode_increment:
        BASICfuck_memory[BASICfuck_memory_index] += argument;
        goto lfinish_interpreter_cycle;

    lopcode_decrement:
        BASICfuck_memory[BASICfuck_memory_index] -= argument;
        goto lfinish_interpreter_cycle;

    lopcode_bfmemory_left:
        if (BASICfuck_memory_index > argument) {
            BASICfuck_memory_index -= argument;
        } else {
            BASICfuck_memory_index = 0;
        }
        goto lfinish_interpreter_cycle;

    lopcode_bfmemory_right:
        if (BASICfuck_memory_index + argument < BASICFUCK_MEMORY_SIZE)
            BASICfuck_memory_index += argument;
        goto lfinish_interpreter_cycle;

    lopcode_print:
        (void)putchar(BASICfuck_memory[BASICfuck_memory_index]);
        goto lfinish_interpreter_cycle;

    lopcode_input:
        BASICfuck_memory[BASICfuck_memory_index] = cgetc();
        goto lfinish_interpreter_cycle;

    lopcode_jeq:
        if (BASICfuck_memory[BASICfuck_memory_index] == 0) {
            // Since the program can only be 256 bytes long, we can ignore
            // the high byte of the address.
            program_index = argument;
        }
        goto lfinish_interpreter_cycle;

    lopcode_jne:
        if (BASICfuck_memory[BASICfuck_memory_index] != 0) {
            // Since the program can only be 256 bytes long, we can ignore
            // the high byte of the address.
            program_index = argument;
        }
        goto lfinish_interpreter_cycle;

    lopcode_cmemory_read:
        BASICfuck_memory[BASICfuck_memory_index] = *computer_memory_pointer;
        goto lfinish_interpreter_cycle;

    lopcode_cmemory_write:
        *computer_memory_pointer = BASICfuck_memory[BASICfuck_memory_index];
        goto lfinish_interpreter_cycle;

    lopcode_cmemory_left:
        if ((uint)computer_memory_pointer > argument) {
            computer_memory_pointer -= argument;
        } else {
            computer_memory_pointer = 0;
        }
        goto lfinish_interpreter_cycle;

    lopcode_cmemory_right:
        if (UINT_MAX - (uint)computer_memory_pointer > argument) {
            computer_memory_pointer += argument;
        } else {
            computer_memory_pointer = (uchar*)UINT_MAX;
        }
        goto lfinish_interpreter_cycle;

    lopcode_execute:
        execute();
        goto lfinish_interpreter_cycle;


    lfinish_interpreter_cycle:
        // Jumped to after an opcode has been executed.
        program_index += opcode_size_table[opcode];
    }
}



/**
 * Runs the help menu, telling the user about the REPL and it's functions.
 */
void help_menu(void) {
    clrscr();
    (void)puts("REPL Commands (must be at start of line):\n"
               "\n"
               "! - Exits REPL.\n"
               "? - Displays this help menu.\n"
               "# - Displays bytecode of last program.\n"
               "\n"
               "REPL Controls (Keypress):\n"
               "\n"
               "STOP - Cancel input and start new line.\n"
               "       Like C-c.\n"
               "HOME - Move to start of line.\n"
               "CLR - Clear screen and line.\n"
               "\n"
               "F1 - Abort BASICfuck program.\n"
               "\n"
               "Press ANY KEY to CONTINUE");
    (void)cgetc();

    clrscr();
    (void)puts("BASICfuck Instructions (Part 1):\n"
               "\n"
               "+ - Increment cell.\n"
               "- - Decrement cell.\n"
               "> - Move to next cell.\n"
               "< - Move to previous cell.\n"
               ". - Display value in cell as character.\n"
               ", - Store value of key from keyboard in cell.\n"
               "[ - Jump to corresponding ']' if value of cell is 0.\n"
               "] - Jump to corresponding '[' if value of cell is not 0.\n"
               "\n"
               "Press ANY KEY to CONTINUE");
    (void)cgetc();

    clrscr();
    (void)puts("BASICfuck Instructions (Part 2):\n"
               "\n"
               ") - Move to next location in computer memory.\n"
               "( - Move to previous location in computer memory.\n"
               "@ - Read value from computer memory into cell.\n"
               "* - Write value from cell into computer memory\n"
               "% - Execute location in computer memory as subroutine (currently disabled.) The values of the current and next two cells will be used for the A, X, and Y registers. Resulting register values will be stored back into the respective cells.\n"
               "\n"
               "Press ANY KEY to CONTINUE");
    (void)cgetc();

    clrscr();
}

// TODO switch to using ctrl key for slow down.
/**
 * Displays a readout of the bytecode of the last program to the user. Holding
 * space will slow down the printing.
 *
 * @param program (global) - the program buffer.
 */
void display_bytecode() {
    uchar i = 0;

    // Ideally display 16 bytes at a time, but screen real estate is what it is.
    uchar bytes_per_line = (screen_width - 7) / 3;
    bytes_per_line = bytes_per_line > 16 ? 16 : bytes_per_line;

    while (true) {
        if (i % bytes_per_line == 0) {
            // Slow down while holding space.
            if (kbhit() != 0 && cgetc() == ' ')
                sleep(1);

            // Prints addresses.
            (void)fputs("\n$", stdout);
            cputhex16((uint)i);
            (void)putchar(':');
        }
        // Prints values.
        (void)putchar(' ');
        cputhex8(program_memory[i]);

        if (i >= PROGRAM_MEMORY_SIZE - 1)
            break;
        ++i;
    }

    (void)putchar('\n');
}



#define INPUT_BUFFER_SIZE 256U

uchar input_buffer[INPUT_BUFFER_SIZE];

// TODO change readout of values to have leading 0s for the cell index.
int main(void) {
    // Used to avoid calling the bloated printf.
    uchar number_to_string_buffer[6];

    // Initializes global screen size variables in screen.h.
    screensize(&screen_width, &screen_height);

    // Initializes the opcode table in opcodes.h.
    initialize_instruction_opcode_table();

    // Initializes the interpreter.
    BASICfuck_memory        = calloc(BASICFUCK_MEMORY_SIZE, sizeof(uchar));
    BASICfuck_memory_index  = 0;
    computer_memory_pointer = 0;

    if (BASICfuck_memory == NULL) {
        (void)puts("?INSUFFICIENT MEMORY\n"
                   "\n"
                   "Press ANY KEY to EXIT");
        (void)cgetc();
        return 0;
    }


    clrscr();
    (void)puts("Brainblast-Toolkit BASICfuck REPL 0.1.0\n");
    (void)fputs( utoa(BASICFUCK_MEMORY_SIZE, number_to_string_buffer, 10)
               , stdout);
    (void)puts(" CELLS FREE\n"
               "\n"
               "Enter '?' for HELP\n"
               "Enter '!' to EXIT\n");

    while (true) {
        // Run.
        (void)fputs("YOUR WILL? ", stdout);
        edit_buffer(input_buffer, INPUT_BUFFER_SIZE - 1);

        switch (input_buffer[0]) {
        case NULL:
            continue;                             // empty input.

        case '!':
            (void)puts("SO BE IT.");
            goto lexit_repl;

        case '?':
            help_menu();
            continue;

        case '#':
            display_bytecode();
            continue;
        }

        // Evaluate.
        switch (bytecode_compile(input_buffer, program_memory, PROGRAM_MEMORY_SIZE)) {
        case BCCOMPILE_OUT_OF_MEMORY:
            (void)puts("?OUT OF MEMORY");
            continue;
        case BCCOMPILE_UNTERMINATED_LOOP:
            (void)puts("?UNTERMINATED LOOP");
            continue;
        }

        run_interpreter();

        // Print.
        (void)fputs( utoa(BASICfuck_memory[BASICfuck_memory_index], number_to_string_buffer, 10)
                   , stdout);
        (void)fputs(" (Cell ", stdout);
        (void)fputs( utoa(BASICfuck_memory_index, number_to_string_buffer, 10)
                   , stdout);
        (void)fputs(", Memory $", stdout);
        cputhex16((uint)computer_memory_pointer);
        (void)puts(")");
    }
 lexit_repl:


    return 0;
}
