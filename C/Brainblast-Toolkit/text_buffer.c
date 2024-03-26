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
 * Implementation of text_buffer.h
 */

#include "text_buffer.h"

#include <conio.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#include "keyboard.h"
#include "screen.h"



// The size of the history stack in bytes.
#define HISTORY_STACK_SIZE 2048

/**
 * A stack to store previous inputs so they can be recalled.
 */
uchar history_stack[HISTORY_STACK_SIZE];

/**
 * The next writable index into the history stack.
 */
uint history_stack_index = 0;

/**
 * Saves the given null-terminated text buffer to the history stack for later
 * recollection.
 *
 * @param buffer - the null-terminated buffer to save.
 */
void save_buffer(const uchar *const buffer) {
    uchar character;
    uchar buffer_index = 0;

    while (true) {
        character = buffer[buffer_index];

        history_stack[history_stack_index] = character;
        history_stack_index++;
        if (history_stack_index > HISTORY_STACK_SIZE)
            history_stack_index = 0;

        if (character == NULL)
            break;
        buffer_index++;
    }
}

void recall_buffer(uchar *const buffer) {
    uint history_seek_index = history_stack - 1;

    while (true) {
        if (history_seek_index > 0
    }
}



// TODO add history.
/**
 * Defined in header file.
 */
void edit_buffer(uchar *const buffer, uchar buffer_max_index) {
    uchar new_cursor;
    uchar key;

    // The position of the user inside the buffer.
    uchar cursor = 0;
    // How much of the buffer is taken up by the text typed by the user.
    uchar input_size = 0;
    // Ensures the last byte is reserved for a null-terminator.
    buffer_max_index -= 1;


    while (true) {
        key = blinking_cgetc();

        switch (key) {
        // Finalizes the buffer and exits from this function.
        case KEYBOARD_ENTER:
            buffer[input_size] = NULL;            // write out null-terminator.
            for (; cursor < input_size; cursor++) // navigate to then end of buffer if neccesary.
                (void)putchar(KEYBOARD_RIGHT);
            (void)putchar('\n');

            goto lquit_editing_buffer;

        // "Clears" the input buffer and exits from this function.
        case KEYBOARD_STOP:
            buffer[0] = NULL;                     // write null terminator to start of buffer, "clearing" it.
            (void)putchar('\n');

            goto lquit_editing_buffer;

        // Clears the screen and input buffer.
        case KEYBOARD_CLEAR:
            cursor     = 0;
            input_size = 0;
            clrscr();
            break;

        // Deletes characters from the buffer.
        case KEYBOARD_BACKSPACE:
            if (cursor == 0)
                break;

            (void)putchar(KEYBOARD_BACKSPACE);    // display backspace.
            // Shifts characters in buffer to the left, overwiting the deleted
            // character.
            (void)memmove(buffer+cursor - 1, buffer+cursor, input_size - cursor);
            --input_size;
            --cursor;

            break;

        // Handles arrow keys, moving through the buffer.
        case KEYBOARD_LEFT:
            if (cursor > 0) {
                --cursor;
                (void)putchar(KEYBOARD_LEFT);
            }
            break;

        case KEYBOARD_RIGHT:
            if (cursor < input_size) {
                ++cursor;
                (void)putchar(KEYBOARD_RIGHT);
            }
            break;

        case KEYBOARD_UP:
            // Navigates to the next line up, or to the start of the buffer, if
            // there is no line there.
            new_cursor = cursor > screen_width ? cursor - screen_width : 0;
            for (; cursor > new_cursor; --cursor)
                (void)putchar(KEYBOARD_LEFT);

            break;

        case KEYBOARD_DOWN:
            // Navigates to the next line down, or to the end of the filled
            // buffer, if there is no line there.
            new_cursor = (input_size - cursor) > screen_width ? cursor + screen_width : input_size;
            for (; cursor < new_cursor; ++cursor)
                (void)putchar(KEYBOARD_RIGHT);

            break;

        // Handles HOME, moving to the start of the buffer.
        case KEYBOARD_HOME:
            for (; cursor > 0; --cursor)
                (void)putchar(KEYBOARD_LEFT);
            break;

        // Handles INST, inserting characters into the buffer.
        case KEYBOARD_INSERT:
            if (input_size > buffer_max_index || cursor == input_size)
                break;

            (void)putchar(KEYBOARD_INSERT);       // display insertion.
            // Shifts characters in buffer to the right, making space for the
            // new one.
            (void)memmove(buffer+cursor + 1, buffer+cursor, input_size - cursor);
            input_size++;
            buffer[cursor] = ' ';

            break;

        // Handles function keys, navigating through the history buffer.
        case KEYBOARD_F1:
            for (; cusor > 0; --cursor)
                (void)putchar(KEYBOARD_BACKSPACE);

        // Handles typing characters.
        default:
            if ((key & 0x7F) < 0x20)              // filter out unhandled control characters.
                break;
            if (cursor > buffer_max_index)
                break;

            if (cursor == input_size)             // increase buffer size if adding characters to the end.
                ++input_size;
            buffer[cursor] = key;
            ++cursor;
            (void)putchar(key);
        }
    }
 lquit_editing_buffer:

    save_buffer(buffer);
    return;
}
