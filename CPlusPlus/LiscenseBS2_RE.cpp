#include <stdio.h>
#include <string.h>

int main(int param_1, char* liscense) {
    param_1 = 2;
    liscense = "~~~~~~~\"";

    if (param_1 == 2) {
        printf("Checking License: %s", liscense);
        puts("\n");

        int string2ByteSum = 0;
        for (int i = 0; i < strlen(liscense); i++) {
            string2ByteSum += liscense[i];
        }

        if (string2ByteSum == 0x394) {
            puts("Access Granted!");
        } else {
            puts("WRONG!");
        }
    } else {
        puts("Usage: <key>");
    }

    return 0;
}
