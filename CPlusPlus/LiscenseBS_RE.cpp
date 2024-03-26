#include <stdio.h>
#include <string.h>

int main(int param_1, char* param_2)
{
    ////////////////////////////////////////
    //             INJECTION              //
    ////////////////////////////////////////
    param_1 = 2;
    param_2 = "AAAA-Z10N-42-OK";
    ////////////////////////////////////////

	int iVar1;

	if (param_1 == 2) {
		printf("Checking License: %s\n", param_2);
		iVar1 = strcmp(param_2, "AAAA-Z10N-42-OK");
		if (iVar1 == 0) {
			puts("Access Granted!");
		}
		else {
			puts("WRONG!");
		}
	}
	else {
		puts("Usage: <key>");
	}
	return 0;
}

