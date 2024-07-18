#pragma once

#include <unordered_map>
#include <string>
#include <stack>

#include "sona_lawa.hpp"

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

/**
 * nimi wawa ni li ken kepeken lon lipu wawa pi toki "ilo li sina."
 */
namespace ilo {
    /**
     * @brief nimi wawa kiwen lon C++ tawa kepeken lon lipu pi toki "ilo li sina".
     */
    typedef void(*NimiWawaKiwen)(SonaLawa&, unsigned int);
    /**
     * @brief nimi wawa tawa kiwen lon C++ tawa kepeken lon li pi toki "ilo li sina".
     */
    typedef void(*NimiWawaTawaKiwen)(SonaLawa&, unsigned int, size_t);
    /**
     * @brief nanpa pi ijo wile li ken nanpa ali la nanpa ni li kepeken.
     */
    extern const unsigned int nanpaPiIjoWileAli;

    class TomoPiNimiWawa {
        public:
            unsigned int nanpaLiliPiIjoWile;
            unsigned int nanpaSuliPiIjoWile;

            /**
             * @param nanpaLiliPiIjoWile lawa() la nanpa nanpaIjo li wile sama anu suli tawa nanpa ni.
             * @param nanpaSuliPiIjoWile lawa() la nanpa nanpaIjo li wile sama anu lili tawa nanpa ni.
             */
            TomoPiNimiWawa(unsigned int nanpaLiliPiIjoWile, unsigned int nanpaSuliPiIjoWile);
            virtual ~TomoPiNimiWawa() = 0;
    };

    /**
     * @brief li poki e nimi wawa en sona lili wile ona sama nanpa pi ijo wile.
     */
    class NimiWawa : public TomoPiNimiWawa {
        public:
            NimiWawaKiwen nimiWawaKiwen = nullptr;

            NimiWawa(NimiWawaKiwen nimiWawaKiwen);
            NimiWawa(unsigned int nanpaLiliPiIjoWile, NimiWawaKiwen nimiWawaKiwen);
            NimiWawa(NimiWawaKiwen nimiWawaKiwen, unsigned int nanpaSuliPiIjoWile);
            NimiWawa( unsigned int nanpaLiliPiIjoWile
                    , NimiWawaKiwen nimiWawaKiwen
                    , unsigned int nanpaSuliPiIjoWile);

            /**
             * @brief li lawa e ilo nanpa kepeken nimi wawa kiwen lon ni. li moku e ijo pi nanpa nanpaIjo
             *      tan poki pokiPali.
             * @attention nanpaIjo li wile lon nasin nanpa (nanpaLiliPiIjoWile-nanpaSuliPiIjoWile).
             *
             * @param nanpaIjo li moku e ijo pi nanpa ni tan pokiPali.
             * @throws std::runtime_error ike li kama.
             */
            void lawa(SonaLawa& sonaLawa, unsigned int nanpaIjo) const noexcept(false);
    };

    class NimiWawaTawa : public TomoPiNimiWawa {
        public:
            NimiWawaTawaKiwen nimiWawaTawaKiwen = nullptr;

            NimiWawaTawa(NimiWawaTawaKiwen nimiWawaTawaKiwen);
            NimiWawaTawa(unsigned int nanpaLiliPiIjoWile, NimiWawaTawaKiwen nimiWawaTawaKiwen);
            NimiWawaTawa(NimiWawaTawaKiwen nimiWawaTawaKiwen, unsigned int nanpaSuliPiIjoWile);
            NimiWawaTawa( unsigned int nanpaLiliPiIjoWile
                        , NimiWawaTawaKiwen nimiWawaTawaKiwen
                        , unsigned int nanpaSuliPiIjoWile);

            /**
             * @brief li lawa e ilo nanpa kepeken nimi wawa tawa kiwen lon ni. li moku e ijo pi nanpa
             *      nanpaIjo tan poki pokiPali. li ken tawa lon ante lon lipu wawa.
             * @attention nanpaIjo li wile lon nasin nanpa (nanpaLiliPiIjoWile-nanpaSuliPiIjoWile).
             *
             * @param nanpaIjo    li moku e ijo pi nanpa ni tan pokiPali.
             * @param lonTawaTawa li ken tawa lon ni.
             * @throws std::runtime_error ike li kama.
             */
            void lawa(SonaLawa& sonaLawa, unsigned int nanpaIjo, size_t lonTawaTawa) const
                    noexcept(false);
    };



    /**
     * @brief li kama jo e nimi wawa tawa pana lon kasi pi kasi lipu kepeken nimi pi nimi wawa tan lipu jan.
     */
    extern const std::unordered_map<std::string, NimiWawa> nimiTawaNimiWawa;

    /**
     * @brief li kama jo e nimi wawa tawa tawa pana lon kasi pi kasi lipu kepeken nimi pi nimi wawa tawa
     *      tan lipu jan.
     */
    extern const std::unordered_map<std::string, NimiWawaTawa> nimiTawaNimiWawaTawa;

    /**
     * @brief li kama jo e nimi pi nimi wawa tawa jan.
     * @throws std::out_of_range nimi wawa kiwen anu nimi wawa tawa kiwen lon tomo li lon ala poki
     *      nimiTawaNimiWawa anu nimiTawaNimiWawaTawa.
     */
    const std::string& tomoPiNimiWawaTawaNimi(const TomoPiNimiWawa* tomoPiNimiWawa) noexcept(false);
}
