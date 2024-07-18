#pragma once

#include <list>
#include <string>
#include <unordered_map>

#include "../kepeken/ike.hpp"

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
 * ilo kipisi tawa kipisi pi lipu wawa pi toki "ilo li sina".
 */
namespace ilo {
    enum class NimiIjo {
        POKI,
        PANA_LON_POKI,

        POKI_NIMI,

        NIMI_WAWA,
        POKI_PI_IJO_PI_NIMI_WAWA,
        NIMI_TAWA_TAWA,

        LINJA_SIN
    };

    /**
     * @brief li ijo tan lipu wawa. lipu kipisi li kipisi lon ni.
     */
    struct Ijo {
        NimiIjo nimiIjo;
        // ijo li ken poki e nimi sama NimiIjo::POKI en NimiIjo::POKI_NIMI la nimi ni li lon ni.
        std::string ijo;
        kepeken::LonIjo lonIjo;

        Ijo(NimiIjo nimiIjo, size_t linja, size_t sitelenLonLinja);

        Ijo(NimiIjo&& nimiIjo, std::string&& ijo, size_t&& linja, size_t&& sitelenLonLinja);

        Ijo(NimiIjo nimiIjo, const std::string& ijo, size_t linja, size_t sitelenLonLinja);
    };



    /**
     * @brief li kipisi e lipu wawa.
     *
     * @param nimiLipu nimi wawa tan lipu wawa anu ante.
     * @param lonLipu  lon pi lipu wawa. tawa toki e ike.
     * @return std::list<Ijo> lipu wawa kipisi.
     * @throw std::runtime_error    ike li kama.
     */
    std::list<Ijo> kipisi(std::istream& lipu, const std::string& lonLipu) noexcept(false);

    /**
     * @brief li kipisi e lipu wawa.
     *
     * @param lonLipu lon pi lipu wawa.
     * @return poki pi ijo pi lipu wawa.
     * @throw std::invalid_argument li ken ala open e lipu.
     * @throw std::runtime_error    ike li kama.
     */
    std::list<Ijo> kipisiELipu(const std::string& lonLipu) noexcept(false);



    /**
     * @brief li kama jo e nimi pi nimi ijo tawa toki tawa jan.
     *
     * @param nimiIjo nimi ijo tawa kama e nimi pi nimi ijo.
     * @return        nimi pi nimi ijo.
     */
    std::string nimiIjoTawaNimiPiNimiIjo(NimiIjo nimiIjo);

    /**
     * @return nimi pi sitelen nasa (jan li sitelen e ni) tan sitelen nasa ni.
     */
    const std::unordered_map<char, char>& sitelenNasaTawaNimi();
}
