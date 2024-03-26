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
 * Screen utilities.
 */

#ifndef _SCREEN_H
#define _SCREEN_H

#include "types.h"



/**
 * The width and height of the screen. Must be initalized at some point with
 * screensize(), or some other method, else they will be set to 0.
 */
extern uchar screen_width
           , screen_height;



/**
 * Runs cgetc() with a blinking cursor.
 *
 * To set a blinking cursor (easily,) you need to use the cursor() function from
 * conio.h, but it seems to error with "Illegal function call" or something when
 * used in a complex function, so I have it pulled out into this separate one.
 *
 * @return the value of the typed character.
 */
uchar blinking_cgetc();



#endif // _SCREEN_H
