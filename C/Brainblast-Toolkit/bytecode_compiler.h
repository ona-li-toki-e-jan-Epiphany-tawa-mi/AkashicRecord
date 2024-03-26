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

/**
 * BASICfuck bytecode compiler.
 */

#ifndef _BYTECODE_COMPILER_H
#define _BYTECODE_COMPILER_H

#include "types.h"



typedef uchar ByteCodeCompileResult;
#define BCCOMPILE_SUCCESS           0
#define BCCOMPILE_OUT_OF_MEMORY     1
#define BCCOMPILE_UNTERMINATED_LOOP 2

/**
 * Bytecode compiles BASICfuck code.
 *
 * @param read_buffer - the null-terminated program text buffer to compile.
 * @param write_buffer - the buffer to write the compiled program to.
 * @param write_buffer_size - the size of the program memory buffer.
 * @return BCCOMPILE_SUCCESS on success,
 *         BCCOMPILE_OUT_OF_MEMORY if the program exceeded the size of the
 *         program memory,
 *         BCCOMPILE_UNTERMINATED_LOOP if the program has an unterminated loop.
 */
ByteCodeCompileResult bytecode_compile(const uchar *const read_buffer, uchar *const write_buffer, const uint write_buffer_size);



#endif // _BYTECODE_COMPILER_H
