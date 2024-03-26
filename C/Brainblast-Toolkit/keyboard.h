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
 * Abstract keyboard interface for the computers supported by cc65.
 *
 * Defines:
 * - KEYBOARD_UP        - up arrow key.
 * - KEYBOARD_DOWN      - down arrow key.
 * - KEYBOARD_LEFT      - left arrow key.
 * - KEYBOARD_RIGHT     - right arrow key.
 * - KEYBOARD_BACKSPACE - backspace/delete key.
 * - KEYBOARD_INSERT    - insert key.
 * - KEYBOARD_ENTER     - enter/return key.
 * - KEYBOARD_STOP      - stop key.
 * - KEYBOARD_HOME      - home key.
 * - KEYBOARD_CLEAR     - clear key.
 * - KEYBOARD_F1        - function key 1. If not present on system, this will be
 *                        mapped to something else.
 * - KEYBOARD_F2        - function key 2. If not present on system, this will be
 *                        mapped to something else.
 *
 * Supported build targets: c16, plus4, c64, c128, pet, cbm510, cbm610, vic20.
 */

#ifndef _KEYBOARD_H
#define _KEYBOARD_H



#if defined(__CBM__)

#include <cbm.h>

#define KEYBOARD_UP        CH_CURS_UP
#define KEYBOARD_DOWN      CH_CURS_DOWN
#define KEYBOARD_LEFT      CH_CURS_LEFT
#define KEYBOARD_RIGHT     CH_CURS_RIGHT
#define KEYBOARD_BACKSPACE CH_DEL
#define KEYBOARD_INSERT    CH_INS
#define KEYBOARD_ENTER     CH_ENTER
#define KEYBOARD_STOP      CH_STOP
#define KEYBOARD_HOME      CH_HOME
#define KEYBOARD_CLEAR     0x93U                  // Not defined in cbm.h, but the same for all CBM computers.

#if defined(__C16__ || __C64__ || __C128__ || __VIC20__)
#define KEYBOARD_F1 CH_F1
#define KEYBOARD_F2 CH_F2
#elif defined(__PET__) // __C16__ || __C64__ || __C128__ || __VIC20__
#define KEYBOARD_F1 95                            // Left arrow character key,
#define KEYBOARD_F2 94                            // Up arrow character key.
#else // __PET__
#error build target not supported
#endif

#else // __CBM__
#error build target not supported
#endif



#endif // _KEYBOARD_H
