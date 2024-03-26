#include <stdio.h>
#include <string.h>
#include <vector>
#include <iostream>

int main() {
    int keySum = 0x394;
    std::vector<char> key;

    std::cout << "Generating key for key sum: 0x" << std::hex << keySum << "\n";
    while (true) {
        if (keySum <= 0) {
            break;
        } else if (keySum >= 0x7e) {
            keySum -= 0x7e;
            key.push_back((char) 0x7e);
        } else {
            key.push_back((char) keySum);
            break;
        }
    }

    printf("Generated key: (");
    for (int i = 0; i < key.size(); i++) {
        printf("%c", key.at(i));
    }
    puts(")");

    return 0;
}

