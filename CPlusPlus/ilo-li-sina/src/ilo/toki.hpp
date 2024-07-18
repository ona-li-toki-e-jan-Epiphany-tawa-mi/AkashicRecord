#include "pali/pali.hpp"

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
 * ilo tawa toki e ijo pi nimi wawa tawa jan.
 */
namespace ilo {
    /**
     * @brief li toki e ijo pi lipu wawa tawa jan pi lawa e ilo nanpa.
     *
     * @param ijoLipu ijoLipu tan kipisi().
     * @param lonLipu lon pi lipu wawa.
     */
    void tokiELipuKipisi(const std::list<Ijo>& ijoLipu, const std::string& lonLipu);

    /**
     * @brief li pana e lipu wawa lon nimi li toki e ona lon ilo pi pana nimi.
     *
     * @param kasiOpen lipu wawa pali.
     * @param lonLipu  lon pi lipu wawa.
     */
    void tokiELipuPali(const KasiOpen& kasiOpen, const std::string& lonLipu);
}
