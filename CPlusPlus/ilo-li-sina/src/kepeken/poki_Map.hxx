#pragma once

#include <unordered_map>

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
    /**
     * @brief li pali e poki Map sin tan poki ante lon nasin ante.
     * @attention nimi ken lon tenpo 1 taso. ijo mute sama li lon poki la 1 taso li awen.
     *
     * @param nimiIjo nimi ijo lon poki. li kama ijo pi poki sin.
     * @param ijo     ijo pi poki Map. li kama nimi ijo pi poki sin.
     * @param pokiMAP poki tawa pali e ante.
     *
     * @return poki lon nasin ante.
     */
    template<typename nimiIjo, typename ijo>
    std::unordered_map<ijo, nimiIjo> pokiMAPLonNasinAnte(const std::unordered_map<nimiIjo, ijo>& pokiMAP) {
        std::unordered_map<ijo, nimiIjo> pokiMAPSin;

        for (const auto& [nimiIjoPoki, ijoPoki] : pokiMAP)
            pokiMAPSin[ijoPoki] = nimiIjoPoki;

        return pokiMAPSin;
    }
}
