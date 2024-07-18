#pragma once

#include <string>

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
 * ilo tawa ante toki e nimi tawa pana tawa jan.
 * @attention sina wile kepeken e alasaETokiWile() lon monsi pi kepeken pi nimi wawa lon ni.
 */
namespace ante_toki {
    /**
     * @brief li lukin kama jo e toki wile jan li open kepeken e ona tawa ante toki.
     * @attention sina wile kepeken e ni lon monsi pi kepeken pi nimi wawa lon lipu ni.
     *
     * nanpa 1 la li lukin e poki nanpa LANG pi lawa OS. (true)
     * nanpa 2 la toki pona li lon la li kepeken e toki pona. (false)
     * nanpa 3 la li kepeken e toki pi nanpa 1 lon poki pi toki ali. (false)
     * nanpa 4 la toki ala la ala li kama. (false)
     *
     * @return li kama ala kama jo e toki wile.
     */
    bool alasaETokiWile();

    /**
     * @brief li kama jo e nimi pi toki jan tan nimi nimi.
     * @attention sina wile kepeken e alasaETokiWile() lon monsi kepeken.
     *
     * li ken ala kama jo e nimi tawa jan la li pana e nimi nimi pana.
     *
     * @param nimiNimi nimi tawa kama jo e nimi tawa jan (sama "examle.test" anu "ijo_suli.jan_suli.nimi").
     *
     * @return          nimi tawa jan.
     * @retval nimiNimi li ken ala kama jo e nimi tawa jan la li pana.
     */
    const std::string& nimiTawaJan(const std::string& nimiNimi);

    /**
     * @brief li ante e nimi weka tawa nimi kama lon poki nimi.
     *
     * @param nimiTawaAnte li ante e poki nimi ni.
     * @param nimiWeka     nimi ni li weka tan nimi tawa ante.
     * @param nimiKama     nimi ni li kama lon lon pi nimi weka lon nimi tawa ante.
     *
     * @return              nimi sin pi nimi weka ala pi nimi kama kin.
     * @retval nimiTawaAnte nimi weka li lon ala nimi tawa ante.
     */
    std::string anteENimi(std::string nimiTawaAnte, const std::string& nimiWeka, const std::string& nimiKama);



    /**
     * @brief li kama jo e suli e nanpa Byte tawa sitelen UTF-8 1.
     *
     * @param pokiNimi li lukin lon poki ni.
     * @param open     open sitelen li lon ni.
     * @param pini     li pini lukin lon ni. li pana ala la li pini lon pini poki.
     *
     * @return li pona                      la suli pi sitelen UTF-8.
     *         li open pi sitelen ike UTF-8 la suli * -1.
     *         li pini pi sitelen UTF-8     la 0
     */
    int UTF8LaSuliSitelen(const std::string& pokiNimi, const size_t open);

    /**
     * @brief li kama jo e nanpa sitelen pana lon insa pi open en pini tan poki nimi UTF-8.
     *
     * @param pokiNimi li nanpa e nanpa sitelen pana lon poki ni.
     * @param open     li open nanpa e sitelen lon ni.
     * @param pini     li pini nanpa e sitelen lon ni.
     *
     * @return nanpa sitelen pana tan poki nimi UTF-8.
     */
    size_t UTF8LaNanpaSitelen(const std::string& pokiNimi, size_t open, const size_t pini);

    /**
     * @brief li kama jo e nanpa pi nanpa Byte tawa nanpa sitelen pana. li open nanpa e ona tan open.
     * @attention sina wile kepeken e alasaETokiWile() lon monsi kepeken. ni ala la li sama std::string::size().
     *
     * @param pokiNimi     li nanpa e nanpa Byte lon poki ni.
     * @param open         li open nanpa lon ni.
     * @param nanpaSitelen li nanpa e nanpa Byte pi sitelen pi nanpa ni.
     *
     * @return nanpa pi nanpa Byte tawa nanpa sitelen pana.
     */
    size_t UTF8LaNanpaBYTE(const std::string& pokiNimi, size_t open, size_t nanpaSitelen);


    //void ponaESitelenUTF8(std::string& pokiSitelen);
}
