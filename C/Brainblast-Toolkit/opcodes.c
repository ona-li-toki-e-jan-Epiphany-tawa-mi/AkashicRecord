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
 * Implementation of opcodes.h
 */

#include "opcodes.h"

#include <stddef.h>



/**
 * Defined in header file.
 */
const uchar opcode_size_table[] = {
    1,                                            // BASICFUCK_END_PROGRAM.
    2,                                            // BASICFUCK_INCREMENT.
    2,                                            // BASICFUCK_DECREMENT.
    2,                                            // BASICFUCK_BFMEMORY_LEFT.
    2,                                            // BASICFUCK_BFMEMORY_RIGHT.
    1,                                            // BASICFUCK_PRINT.
    1,                                            // BASICFUCK_INPUT.
    3,                                            // BASICFUCK_JEQ.
    3,                                            // BASICFUCK_JNE.
    1,                                            // BASICFUCK_CMEMORY_READ.
    1,                                            // BASICFUCK_CMEMORY_WRITE.
    2,                                            // BASICFUCK_CMEMORY_LEFT.
    2,                                            // BASICFUCK_CMEMORY_RIGHT.
    1                                             // BASICFUCK_EXECUTE.
};



/**
 * Defined in header file.
 */
uchar instruction_opcode_table[256];

/**
 * Defined in header file.
 */
void initialize_instruction_opcode_table() {
    uchar i = 0;
    for (; i < 255; i++)
        instruction_opcode_table[i] = 0xFF;

    instruction_opcode_table[NULL] = BASICFUCK_END_PROGRAM;
    instruction_opcode_table['+']  = BASICFUCK_INCREMENT;
    instruction_opcode_table['-']  = BASICFUCK_DECREMENT;
    instruction_opcode_table['<']  = BASICFUCK_BFMEMORY_LEFT;
    instruction_opcode_table['>']  = BASICFUCK_BFMEMORY_RIGHT;
    instruction_opcode_table['.']  = BASICFUCK_PRINT;
    instruction_opcode_table[',']  = BASICFUCK_INPUT;
    instruction_opcode_table['[']  = BASICFUCK_JEQ;
    instruction_opcode_table[']']  = BASICFUCK_JNE;
    instruction_opcode_table['@']  = BASICFUCK_CMEMORY_READ;
    instruction_opcode_table['*']  = BASICFUCK_CMEMORY_WRITE;
    instruction_opcode_table['(']  = BASICFUCK_CMEMORY_LEFT;
    instruction_opcode_table[')']  = BASICFUCK_CMEMORY_RIGHT;
    instruction_opcode_table['%']  = BASICFUCK_EXECUTE;
}
