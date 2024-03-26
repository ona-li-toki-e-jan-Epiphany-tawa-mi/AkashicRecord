#include <fstream>
#include <iostream>
#include <sstream>

const std::string inputFileName("FahrenheitTemperature.txt");
const std::string outputFileName("CelsiusTemperature.txt");

int fahrenheitToCelsius(int degreesFahrenheit) {
    return static_cast<int>(static_cast<double>(degreesFahrenheit - 32) * (5.0/9.0));
}

int main(void) {
    // Using RAII is, as it's called, an "industry best practice."
    std::ifstream input(inputFileName);
    std::ofstream output(outputFileName);

    std::string line;
    while (std::getline(input, line)) {
        std::string city;
        int degreesFahrenheit;

        std::stringstream lineStream(line);
        lineStream >> city >> degreesFahrenheit;

        output << city << ' ' << fahrenheitToCelsius(degreesFahrenheit) << '\n';
    }

    return 0;
}