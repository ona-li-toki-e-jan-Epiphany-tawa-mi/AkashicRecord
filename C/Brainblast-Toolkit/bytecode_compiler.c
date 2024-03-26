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
 * Implementation of bytecode_compiler.h.
 */

#include "bytecode_compiler.h"

#include <stddef.h>
#include <stdbool.h>

#include "opcodes.h"



// Global variables for passing parameters between compiler functions.
const uchar* compiler_read_buffer;                // the null-terminated buffer to read the program to compile from.
uint         compiler_read_index;                 // an index into the read buffer.
uchar*       compiler_write_buffer;               // the buffer to write the compiled program to.
uint         compiler_write_index;                // an index into the write buffer.
uint         compiler_write_buffer_size;          // the size of the write buffer.



/**
 * Performs the first pass of BASICfuck compilation, converting the text program
 * to opcodes.
 *
 * @param compiler_read_buffer (global) - the read buffer.
 * @param compiler_read_index (global) - the current index into the read buffer.
 * @param compiler_write_buffer (global) - the write buffer.
 * @param compiler_write_index (global) - the current index into the write buffer.
 * @param compiler_write_buffer_size (global) - the size of the write buffer.
 * @return true if succeeded, false if ran out of memory.
 */
bool compile_first_pass() {
    uchar instruction;
    uchar opcode;

    // Used by counted instructions.
    uint  instruction_count;
    uchar other_instruction;
    uchar chunk_count;

    static const void *const jump_table[] = {
        &&lfinish_bytecode_compilation,           // BASICFUCK_END_PROGRAM.
        &&lcompile_counted_instruction,           // BASICFUCK_INCREMENT.
        &&lcompile_counted_instruction,           // BAISCFUCK_DECREMENT.
        &&lcompile_counted_instruction,           // BASICFUCK_BFMEMORY_LEFT.
        &&lcompile_counted_instruction,           // BASICFUCK_BFMEMORY_RIGHT.
        &&lcompile_instruction_no_arugments,      // BASICFUCK_PRINT.
        &&lcompile_instruction_no_arugments,      // BASICFUCK_INPUT.
        &&lcompile_jump_instruction,              // BASICFUCK_JEQ.
        &&lcompile_jump_instruction,              // BASICFUCK_JNE.
        &&lcompile_instruction_no_arugments,      // BASICFUCK_CMEMORY_READ.
        &&lcompile_instruction_no_arugments,      // BASICFUCK_CMEMORY_WRITE.
        &&lcompile_counted_instruction,           // BASICFUCK_CMEMORY_LEFT.
        &&lcompile_counted_instruction,           // BASICFUCK_CMEMORY_RIGHT.
        &&lcompile_instruction_no_arugments       // BASICFUCK_EXECUTE.
    };

    while (true) {
        instruction = compiler_read_buffer[compiler_read_index];
        opcode      = instruction_opcode_table[instruction];

        // Ignores non-instructions.
        if (opcode == 0xFF) {
            compiler_read_index++;
            continue;
        }

        goto *jump_table[opcode];


        // End of program.
    lfinish_bytecode_compilation:
        compiler_write_buffer[compiler_write_index] = BASICFUCK_END_PROGRAM;
        break;

        // Takes no arguments.
    lcompile_instruction_no_arugments:
        if (compiler_write_index >= compiler_write_buffer_size)
            return false;

        compiler_write_buffer[compiler_write_index] = opcode;
        ++compiler_read_index;
        ++compiler_write_index;

        continue;

        // Takes a 16-bit address relative to program memory as a parameter,
        // which will be handled by the second pass.
    lcompile_jump_instruction:
        if (compiler_write_index + 2 >= compiler_write_buffer_size)
            return false;

        compiler_write_buffer[compiler_write_index]   = opcode;
        compiler_write_buffer[compiler_write_index+1] = 0xFF;
        compiler_write_buffer[compiler_write_index+2] = 0xFF;
        ++compiler_read_index;
        compiler_write_index += 3;

        continue;

        // Takes an 8-bit count of how many times to preform the operation.
    lcompile_counted_instruction:
        instruction_count = 0;

        // Count number of consecutive instructions.
        while (true) {
            other_instruction = compiler_read_buffer[compiler_read_index];

            if (other_instruction != instruction)
                break;

            ++instruction_count;
            ++compiler_read_index;
        }

        // Each instruction opcode can only take an 8-bit value, so this chops up
        // the full count into separate 8-bit chunks.
        while (instruction_count > 0) {
            if (compiler_write_index + 1 >= compiler_write_buffer_size)
                return false;

            chunk_count = instruction_count > 255 ? 255 : (uchar)instruction_count;

            compiler_write_buffer[compiler_write_index]   = opcode;
            compiler_write_buffer[compiler_write_index+1] = chunk_count;
            compiler_write_index += 2;

            instruction_count -= (uint)chunk_count;
        }

        continue;
    }


    return true;
}



/**
 * Performs the second pass of BASICfuck compilation, calculating the addresses
 * for jump instructions.
 *
 * @param write_buffer (global) - the write buffer.
 * @param compiler_state - the current state of the compiler.
 * @return true if succeeded, false if there is an unterminated loop.
 */
bool compile_second_pass() {
    uint  loop_depth;
    uint  seek_index;
    uchar opcode;
    uchar seeked_opcode;

    compiler_write_index = 0;


    while ((opcode = compiler_write_buffer[compiler_write_index]) != BASICFUCK_END_PROGRAM) {
        switch (opcode) {
        case BASICFUCK_JEQ:
            seek_index = compiler_write_index + opcode_size_table[BASICFUCK_JEQ];
            loop_depth = 1;

            // Finds and links with accomanying JNE instruction.
            while ((seeked_opcode = compiler_write_buffer[seek_index]) != BASICFUCK_END_PROGRAM) {
                switch (seeked_opcode) {
                case BASICFUCK_JEQ:
                    ++loop_depth;
                    break;
                case BASICFUCK_JNE:
                    --loop_depth;
                    break;
                }

                if (loop_depth == 0) {
                    // Sets JEQ instruction to jump to accomanying JNE.
                    *(uint*)(compiler_write_buffer + compiler_write_index+1) = seek_index;
                    // And vice-versa.
                    *(uint*)(compiler_write_buffer + seek_index+1) = compiler_write_index;

                    break;
                }

                seek_index += opcode_size_table[seeked_opcode];
            }

            if (loop_depth != 0)
                return false;

            break;

        case BASICFUCK_JNE:
            // Address should have been set by some preceeding JEQ instruction.
            if (*(uint*)(compiler_write_buffer + compiler_write_index+1) == 0xFFFF)
                return false;

            break;
        }

        compiler_write_index += opcode_size_table[opcode];
    }


    return true;
}



/**
 * Defined in header file.
 */
ByteCodeCompileResult bytecode_compile(const uchar *const read_buffer, uchar *const write_buffer, const uint write_buffer_size) {
    compiler_read_buffer       = read_buffer;
    compiler_read_index        = 0;
    compiler_write_buffer      = write_buffer;
    compiler_write_index       = 0;
    // The last location is reserved for end of program.
    compiler_write_buffer_size = write_buffer_size - 1;

    if (!compile_first_pass())
        return BCCOMPILE_OUT_OF_MEMORY;

    if (!compile_second_pass())
        return BCCOMPILE_UNTERMINATED_LOOP;

    return BCCOMPILE_SUCCESS;
}
