#include "lawa_OS.hpp"

#include <array>
#include <cstring>
#include <optional>

#ifdef WINDOWS
#include <memory>
#include <windows.h>
#include <lmcons.h>
#elif UNIX
#include <unistd.h>
#include <term.h>
#include <pwd.h>
#endif

/*
 * MIT License
 *
 * Copyright (c) 2022 ona-li-toki-e-jan-Epiphany-tawa-mi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

namespace kepeken {
    bool alaEIloPana() {
#ifdef WINDOWS
{
        HANDLE                     lupaSTDOUT;
        CONSOLE_SCREEN_BUFFER_INFO sonaPiIloPana;
        DWORD                      nanpaPiPokiSitelen;
        DWORD                      nanpaSitelen;
        static COORD               lonOpenLonIloPana = {0, 0};

        lupaSTDOUT = GetStdHandle(STD_OUTPUT_HANDLE);
        if (lupaSTDOUT == INVALID_HANDLE_VALUE
                || !GetConsoleScreenBufferInfo(lupaSTDOUT, &sonaPiIloPana))
            return false;

        // li pali e ni: poki sitelen ali li kama ' ' tawa ni: ilo pana li kama ala.
        nanpaPiPokiSitelen = sonaPiIloPana.dwSize.X * sonaPiIloPana.dwSize.Y;
        if (!FillConsoleOutputCharacter( lupaSTDOUT
                                       , ' '
                                       , nanpaPiPokiSitelen
                                       , lonOpenLonIloPana
                                       , &nanpaSitelen))
            return false;

        // li awen e ijo pi lukin pona tan ni: FillConsoleOutputCharacter() li weka e ona.
        if (!FillConsoleOutputAttribute( lupaSTDOUT
                                       , sonaPiIloPana.wAttributes
                                       , nanpaPiPokiSitelen
                                       , lonOpenLonIloPana
                                       , &nanpaSitelen))
            return false;

        // li pali e ni: lon toki (sitelen tan toki() en nimi wawa ante li kama lon ni) li kama lon open pi
        //      ilo pana.
        return SetConsoleCursorPosition(lupaSTDOUT, lonOpenLonIloPana);
}
#elif UNIX
{
        // ilo Terminfo li open ala la li open e ona.
        if (cur_term == nullptr) {
            int nanpaIke;
            setupterm(nullptr, STDOUT_FILENO, &nanpaIke);

            if (nanpaIke <= 0)
                return false;
        }

        // li kama jo e nimi wawa "clear" li lawa e ona.
        return putp(tigetstr("clear")) == 0;
}
#endif

        return false;
    }



    const std::string& nimiJan() {
        static std::optional<std::string> nimiJan = std::nullopt;
        if (nimiJan.has_value())
            return *nimiJan;


    // li lukin kama jo e nimi tan lawa OS.
#ifdef WINDOWS
{
        DWORD suliNimi  = UNLEN + 1;
        auto nimiJanKen = std::make_unique<CHAR[]>(suliNimi);

        if (GetUserNameA(nimiJanKen.get(), &suliNimi))
            return *(nimiJan = std::string(nimiJanKen.get(), suliNimi));
}
#elif UNIX
{
        passwd* tomoPiSonaJan = getpwuid(geteuid());

        if (tomoPiSonaJan != nullptr)
            return *(nimiJan = tomoPiSonaJan->pw_name);
}
#endif


        // li lukin kama jo e ona tan poki Enviroment Variables pi lawa OS.
        static const std::array<std::string, 3> pokiOSPiNimiJan = {"USER", "USERNAME", "LOGNAME"};

        for (auto& poki : pokiOSPiNimiJan) {
            const char* nimiJanKen = getenv(poki.c_str());

            if (nimiJanKen != nullptr && strcmp(nimiJanKen, "") != 0)
                return *(nimiJan = nimiJanKen);
        }


        // nasin ante li pali ala la mi wile pana e ala.
        return *(nimiJan = "");
    }
}
