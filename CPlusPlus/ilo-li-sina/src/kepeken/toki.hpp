#pragma once

#include <iostream>

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
 * @brief ilo tawa toki e ijo pi lukin pona.
 */
namespace kepeken {
    /**
     * Copyright (c) 2003-2012 Michael E. Smoot
     * Copyright (c) 2004-2016 Daniel Aarno
     * Copyright (c) 2017-2021 Google LLC
     *
     * Permission is hereby granted, free of charge, to any person
     * obtaining a copy of this software and associated documentation
     * files (the "Software"), to deal in the Software without restriction,
     * including without limitation the rights to use, copy, modify, merge,
     * publish, distribute, sublicense, and/or sell copies of the Software,
     * and to permit persons to whom the Software is furnished to do so,
     * subject to the following conditions:
     *
     * The above copyright notice and this permission notice shall be
     * included in all copies or substantial portions of the Software.
     *
     * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
     * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
     * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
     * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
     * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
     * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
     * IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
     * THE SOFTWARE.
     *
     * ---------------
     *
     *  The given copyright notice applies only to the following function, originally void(fmtPrintLine)(
     *      std::ostream&, const std::string&, int, int, int), pulled from TCLAP
     *      (http://tclap.sourceforge.net/index.html) - MIT licensed. This message is not apart of
     *      the notice itself.
     *
     * @brief li toki e nimi kepeken linja pi lukin pona.
     *
     * @param lupaTawaToki         li toki e ijo lon lupa ni.
     * @param nimi                 li toki e nimi ni.
     * @param nanpaPiSitelenAla    nanpa pi sitelen ' ' pi lukin ala lon monsi pi linja ali.
     * @param nanpaPiSitelenAlaSin nanpa pi sitelen ' ' sin pi lukin ala lon monsi pi linja pi nanpa 1 ala.
     */
    void tokiELinjaPiLukinPona(std::ostream& lupaTawaToki, const std::string& nimi, const int nanpaPiSitelenAla, int nanpaPiSitelenAlaSin);
}
