/**
 * Computer macros.
 */

#include <chrono>
#include <thread>
#include <time.h>
#include "macro/keyboard_macro.h"
#include "macro_loader.h"

// TODO Label components. When one is executed, all following activators are activated. Resets at end of file or when another is activated.
// TODO Pass components. When one is executed, it does nothing. When one is activated, it activates the macro, no questions asked.
// TODO Add a way to hide the terminal.

int main() {
	KeyboardListener::initialize();
	KeyboardMacros::initialize();
	MacroLoader::loadMacrosFromFile();


	clock_t lastTime = clock();

	while (true) {
		double deltaTime = (clock() - static_cast<double>(lastTime)) / CLOCKS_PER_SEC;
		lastTime = clock();

		KeyboardListener::tick();
		MacroLoader::tick(deltaTime);

		if (deltaTime < 0.03)
			std::this_thread::sleep_for(std::chrono::duration<double>(0.03 - deltaTime));
	}


	KeyboardMacros::uninitialize();
	KeyboardListener::uninitialize();
	MacroLoader::unloadMacros();

	return 0;
}