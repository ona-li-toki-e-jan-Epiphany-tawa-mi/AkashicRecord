#include <stdio.h>
#include <locale.h>

#include <endian.h>

#include <ncurses.h>



// The maxiumum length of a TLV value in bytes.
#define MAX_VALUE_LENGTH 1024

// ncurses functions -> tag values.
#define INITSCR 0
#define PRINTW 1
#define REFRESH 2
#define GETCH 3
#define ENDWIN 4
#define CLEAR 5



void handle_command(const uint32_t tag, const uint32_t length, uint8_t* value) {
    switch (tag) {
    case INITSCR:
        (void)initscr();
        break;
    case PRINTW:
        (void)printw("%s", value);
        break;
    case REFRESH:
        (void)refresh();
        break;
    case GETCH:
        (void)getch();
        break;
    case ENDWIN:
        (void)endwin();
        break;
    case CLEAR:
        (void)endwin();
        break;
    default:
        break;
    };
}

int main(void) {
    uint8_t value_buffer[MAX_VALUE_LENGTH+1];

    // Initializes locale for ncurses to use.
    (void)setlocale(LC_ALL, "");

    // We use TLV to read in the commands for ncurses. The tag is the command,
    // and the value contains the arguments.
    while (!feof(stdin)) {
        if (0 != ferror(stdin)) {
            perror("Error reading from stdin");
            return 1;
        }

        uint32_t tag, length;
        if (1 != fread(&tag,    sizeof(uint32_t), 1, stdin)) continue;
        if (1 != fread(&length, sizeof(uint32_t), 1, stdin)) continue;
        // Tag and length values are big endian, so we need to convert them to
        // the byte order of our host.
        tag    = be32toh(tag);
        length = be32toh(length);

        // If the value is too long we discard it.
        if (length > MAX_VALUE_LENGTH) {
            (void)fseek(stdin, length, SEEK_CUR);
            continue;
        }

        if (length > 0 && length != fread(value_buffer, sizeof(uint8_t), length, stdin))
            continue;
        // Sets last value to null-byte so it can be used as a c-string.
        value_buffer[length] = 0;

        handle_command(tag, length, value_buffer);
    }

    return 0;
}
