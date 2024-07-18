#pragma once

#include <unordered_map>
#include <stack>

#include "../../kepeken/ike.hpp"

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
 * ijo pona tawa lawa.
 */
namespace ilo {
    /**
     * @brief ilo lawa la sona ni li wile lon mute. ni li pona e pana pi sona ni tawa ona.
     */
    struct SonaLawa {
        // lon pi lipu wawa.
        const std::string& lonLipu;

        // nimi pi poki ali en poki pali.
        std::unordered_map<std::string, std::string>& pokiAli;
        // poki pali tawa pana e ijo tawa nimi wawa en pana lon poki.
        std::stack<std::string>&                      pokiPali;
        // kasi pi tenpo ni lon kasi open.
        size_t&                                       kasiPiTenpoNi;
        // ike la ni li -1 ala la li tawa lon ni.
        size_t&                                       ikeLaLonTawaTawa;
        /**
         * @brief lon pi nimi wawa.
         * @attention nimi wawa anu nimi wawa tawa la o pana e ijo lon ni.
         */
        const kepeken::LonIjo*                        lonPiKasiPiTenpoNi;
    };

    /**
     * @brief tenpo lawa la li pali e ike tawa ilo lawa.
     *
     * @param ike sona pi kama ike en tan ike.
     * @throw std::runtime_error ike li kama.
     */
    void paliEIke(const SonaLawa& sonaLawa, const kepeken::IjoPiTokiIke& ike) noexcept(false);
}
