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
 * Defines the opcodes for BASICfuck bytecode.
 */

#ifndef _OPCODES_H
#define _OPCODES_H

#include "types.h"



typedef uchar Opcode;
// Ends the current BASICfuck program.
#define BASICFUCK_END_PROGRAM 0x00U
// Increments the current cell.
// argument1 - the amount to increment by.
#define BASICFUCK_INCREMENT 0x01U
// Decrements the current cell.
// argument1 - the amount to decrement by.
#define BASICFUCK_DECREMENT 0x02
// Moves the the cell pointer to the left.
// argument1 - the number of times to move to the left.
#define BASICFUCK_BFMEMORY_LEFT 0x03U
// Moves the the cell pointer to the right.
// argument1 - the number of times to move to the right.
#define BASICFUCK_BFMEMORY_RIGHT 0x04U
// Prints the value in the current cell as PETSCII character.
#define BASICFUCK_PRINT 0x05U
// Awaits a value from the keyboard and stores it in the current cell.
#define BASICFUCK_INPUT 0x06U
// Jumps to the given address if the current cell is 0.
// argument1,2 - the address in program memory to jump to.
#define BASICFUCK_JEQ 0x07U
// Jumps to the given address if the current cell is not 0.
// argument1,2 - the address in program memory to jump to.
#define BASICFUCK_JNE 0x08U
// Reads the value at the computer memory pointer into the current cell.
#define BASICFUCK_CMEMORY_READ 0x09U
// Writes the value in the current cell to the location at the computer memory
// pointer.
#define BASICFUCK_CMEMORY_WRITE 0x0AU
// Moves the computer memory pointer to the left.
// argument1 - the number of times to move to the left.
#define BASICFUCK_CMEMORY_LEFT 0x0BU
// Moves the computer memory pointer to the right.
// argument1 - the number of times to move to the right.
#define BASICFUCK_CMEMORY_RIGHT 0x0CU
// Runs the subroutine at the computer memory pointer with the current and next
// two cells as the values for the X, Y, and Z registers.
#define BASICFUCK_EXECUTE 0x0DU



/**
 * A table mapping from opcodes to their size (opcode + arguments) in bytes.
 *
 * Index value must be valid opcode.
 */
extern const uchar opcode_size_table[];



/**
 * A table mapping from instruction characters to their corresponding opcodes.
 *
 * Index value must not exceed 255.
 * Must call initialize_instruction_opcode_table() once prior to use.
 * If the given instruction does not have an opcode, 0xFF will be returned.
 */
extern uchar instruction_opcode_table[];

/**
 * A one-time-call function used to initialize instruction_opcode_table[].
 */
void initialize_instruction_opcode_table();



#endif // _OPCODES_H
