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
 * Implementation of screen.h.
 */

#include "screen.h"

#include <stdbool.h>

#include "conio.h"



/**
 * Defined in header file.
 */
uchar screen_width  = 0
    , screen_height = 0;



/**
 * Defined in header file.
 */
uchar blinking_cgetc() {
    uchar character;

    cursor(true);
    character = cgetc();
    cursor(false);

    return character;
}
