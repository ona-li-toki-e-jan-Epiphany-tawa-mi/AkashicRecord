#include "toki.hpp"
#include "../ante_toki/ante_toki.hpp"

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
     */
    void tokiELinjaPiLukinPona(std::ostream& lupaTawaToki, const std::string& nimi, const int nanpaPiSitelenAla, int nanpaPiSitelenAlaSin) {
        const std::string sitelenPiKipisiNimi(" \n");
        /**
         * @brief nanpa sitelen ken lon sitelen 1 tan tokiELinjaPiLukinPona().
         */
        constexpr size_t suliWilePiLinjaAli = 75;

        size_t suliWileLinja = suliWilePiLinjaAli - nanpaPiSitelenAla;
        std::string sitelenAlaLonMonsi(nanpaPiSitelenAla, ' ');
        size_t tan = 0, tawa = 0;

        while (true) {
            // linja awen li lili tawa suli wile la li ken pona toki e ona li ken pini.
            if (ante_toki::UTF8LaNanpaSitelen(nimi, tan, nimi.size()) <= suliWileLinja) {
                lupaTawaToki << sitelenAlaLonMonsi << nimi.substr(tan) << '\n';
                return;
            }

            size_t suliPiLinjaLonLinja = 0;
            size_t maPiKenAla = tawa;
            // li kipisi e nimi kepeken sitelen kipisi tawa ni: ona li alasa e nanpa nimi ken lon linja 1.
            while (suliPiLinjaLonLinja <= suliWileLinja && maPiKenAla != std::string::npos) {
                tawa = maPiKenAla;
                maPiKenAla = nimi.find_first_of(sitelenPiKipisiNimi, tawa + 1);

                if (maPiKenAla != std::string::npos)
                    suliPiLinjaLonLinja += ante_toki::UTF8LaNanpaSitelen(nimi, tawa, maPiKenAla);
            }

            // li ken ala pana e nimi lon linja 1 la li ken kipisi e ona.
            if (tawa == tan)
                tawa = tan + ante_toki::UTF8LaNanpaBYTE(nimi, tan, suliWileLinja) - 1;

            lupaTawaToki << sitelenAlaLonMonsi << nimi.substr(tan, tawa - tan) << '\n';

            // li weka e nimi kipisi tan open linja.
            for (; sitelenPiKipisiNimi.find(nimi.at(tawa)) != std::string::npos; tawa++) {}
            tan = tawa;

            // li linja pi nanpa 1 ala la li en e sitelen sin pi lukin ala.
            if (nanpaPiSitelenAlaSin != 0) {
                sitelenAlaLonMonsi.append(nanpaPiSitelenAlaSin, ' ');
                suliWileLinja -= nanpaPiSitelenAlaSin;
                nanpaPiSitelenAlaSin = 0;
            }
        }
    }
}
