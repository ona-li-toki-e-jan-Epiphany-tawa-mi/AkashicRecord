OUTPUT_FILE = ncurses-server
GCC_ARGS    = -Wall -Werror -O3

${OUTPUT_FILE}: server.c
	gcc ${GCC_ARGS} $^ -o $@ -lncurses

.PHONY: clean
clean:
	-rm ${OUTPUT_FILE}
