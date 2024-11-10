#!/bin/sh

# Error on unset variables.
set -u



CC=${CC:-cc}
CFLAGS=${CFLAGS:--Wall -Wextra -Wswitch-enum -Wconversion -Werror -pedantic}

SOURCE=tlpin.c
EXECUTABLE=${SOURCE%.c}



# Displays commands.
set -x

# shellcheck disable=SC2086 # We want word spliting.
"$CC" $CFLAGS "$SOURCE" -o "$EXECUTABLE" || exit 1
