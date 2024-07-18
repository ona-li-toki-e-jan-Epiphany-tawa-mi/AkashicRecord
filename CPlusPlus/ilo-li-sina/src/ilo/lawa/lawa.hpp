#pragma once

#include "../pali/pali.hpp"

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
 * @brief ilo tawa lawa e ilo nanpa kepeken lipu wawa pi toki "ilo li sina".
 */
namespace ilo {
    /**
     * @brief li pana e poki open lon poki ali lon tenpo open pi lipu wawa.
     *
     * @param pokiAli poki en nimi lon ona lon tenpo open pi lipu wawa.
     * @param lonLipu lon pi lipu wawa.
     */
    void panaEPokiOpenLonPokiAli( std::unordered_map<std::string, std::string>& pokiAli
                                , const std::string& lonLipu);

    /**
     * @brief li lawa e ilo nanpa kepeken lipu wawa pana. li pali e poki ali pi poki ala. li kepeken ala
     *      e {@see ilo::panaEPokiOpenLonPokiAli};
     *
     * @param lipuWawa kasi pi lipu wawa.
     * @param lonLipu  lon pi lipu wawa.
     * @throws std::runtime_error lawa la ike li kama.
     */
    void lawaELipu(const KasiOpen& lipuWawa, const std::string& lonLipu) noexcept(false);

    /**
     * @brief li lawa e ilo nanpa kepeken lipu wawa pana.
     * @attention o kepeken e {@see ilo::panaEPokiOpenLonPokiAli} tawa pana e poki open lon ona.
     *
     * @param lipuWawa kasi pi lipu wawa.
     * @param pokiAli  poki en nimi lon ona lon tenpo open pi lipu wawa.
     * @param lonLipu  lon pi lipu wawa.
     * @throws std::runtime_error lawa la ike li kama.
     */
    void lawaELipu(const KasiOpen& lipuWawa
             , std::unordered_map<std::string, std::string>& pokiAli
             , const std::string& lonLipu) noexcept(false);
}
