#include "ante_toki.hpp"

#include <algorithm>
#include <iostream>
#include <clocale>
#include <cstdlib>
#include <cassert>

#include "nimi_toki.hxx"

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

namespace ante_toki {
    const std::unordered_map<std::string, std::string>* tokiWile = nullptr;

    bool alasaETokiWile() {
        if (nimiTokiAli.size() == 0)
            return false;

        // li wile tawa kama jo e suli pi poki nimi UTF-8 tawa ijo toki ante.
        if (setlocale(LC_ALL, "") == nullptr)
            setlocale(LC_ALL, "C");

        // li ante e nasin pi toki ijo tawa toki wile.
        try {
            std::locale::global(std::locale(""));

        } catch (const std::runtime_error& liKenAla) {
            std::locale::global(std::locale("C"));
        }

        // lawa OS mute li pana e toki wile lon "LANG". sin la ni li ken ni: jan li pilin e toki pi ilo ni.
        const char *const tokiJan = getenv("LANG");

        if (tokiJan != nullptr) {
            std::string nimiTokiPona = tokiJan;
            // li weka e ijo sama ".utf8". mi mute li wile e nimi toki taso.
            size_t lonPini = nimiTokiPona.find('.');
            if (lonPini != std::string::npos)
                nimiTokiPona.erase(lonPini);

            try {
                tokiWile = &nimiTokiAli.at(nimiTokiPona);
                return true;
            } catch (const std::out_of_range& tokiWileLiLonAla) {}
        }


        // li lukin kama jo e toki kepeken ala tan jan en lawa OS. ni li nasin pi nanpa pini.
        try {
            tokiWile = &nimiTokiAli.at("tok");

        } catch (const std::out_of_range& tokiPonaLiLonAla) {
            // toki wile en toki pona li lon ala la toki ante li lon la li wile kepeken e ona.
            // toki ala li ike.
            tokiWile = &nimiTokiAli.cbegin()->second;
        }


        return false;
    }

    const std::string& nimiTawaJan(const std::string& nimiNimi) {
        try {
            if (tokiWile != nullptr)
                return tokiWile->at(nimiNimi);

        } catch (const std::out_of_range& nimiWileLiLonAla) {}

        return nimiNimi;
    }

    std::string anteENimi(std::string nimiTawaAnte, const std::string& nimiWeka, const std::string& nimiKama) {
        size_t lonNimi = nimiTawaAnte.find(nimiWeka);
        if (lonNimi == std::string::npos)
            return nimiTawaAnte;

        return nimiTawaAnte.replace(lonNimi, nimiWeka.size(), nimiKama);
    }



    /**
     * @brief li kama sona e ni: sitelen UTF-8 pi nanpa Byte mute li pona ala pona.
     *
     * @param pokiNimi li lukin lon poki ni.
     * @param open     open sitelen li lon ni.
     * @param suli     suli sitelen.
     * @return li pona     la suli.
     *         li pona ala la suli * -1.
     */
    int liSitelenPona(const std::string& pokiNimi, const size_t open, unsigned int suli) {
        if (pokiNimi.size() <= open + suli)
            return -static_cast<int>(suli);

        for (int i = 1; i < suli; i++)
            if ((pokiNimi.at(open + i) & 0b11'000000) != 0b10'000000)
                return -static_cast<int>(suli);

        return static_cast<int>(suli);
    }

    int UTF8LaSuliSitelen(const std::string& pokiNimi, const size_t open) {
        assert(open <= pokiNimi.size() && "open alasa lon poki nimi li ken ala suli tawa suli pi poki ni");

        char sitelen = pokiNimi.at(open);

        // sitelen li lon kulupu ASCII anu li sitelen ante pi suli 1 la suli ona li 1.
        if ((sitelen & 0b1'0000000) == 0)
            return 1;


        // li pini pi sitelen UTF-8 pi nanapa Byte mute la li ken ala kama sona e suli. ken la ona li
        //      ike.
        if ((sitelen & 0b11'000000) == 0b10'000000)
            return 0;

        // li open pi sitelen UTF-8 pi nanpa Byte mute la mi wile alasa e suli li kama sona e ni: ona li
        //      jo e nanpa pi nanpa wile lon pini, en, ona li sitelen pini pona (sama 0b10xxxxxx).
        if ((sitelen & 0b0'11'00000) == 0b0'10'00000)
            return liSitelenPona(pokiNimi, open, 2);

        if ((sitelen & 0b0'111'0000) == 0b0'110'0000)
            return liSitelenPona(pokiNimi, open, 3);

        if ((sitelen & 0b0'1111'000) == 0b0'1110'000)
            return liSitelenPona(pokiNimi, open, 4);

        return -1;
    }

    size_t UTF8LaNanpaSitelen(const std::string& pokiNimi, size_t open, const size_t pini) {
        assert(open <= pokiNimi.size() && pini <= pokiNimi.size() && "open alasa en pini alasa lon poki nimi li ken ala suli tawa suli pi poki ni");
        assert(open <= pini && "open alasa li ken ala suli tawa pini alasa");

        size_t suli = 0;

        while (open < pini) {
            int suliSitelen = std::abs(UTF8LaSuliSitelen(pokiNimi, open));
            if (suliSitelen == 0)
                suliSitelen = 1;

            open += static_cast<size_t>(suliSitelen);
            suli++;
        }

        return suli;
    }

    size_t UTF8LaNanpaBYTE(const std::string& pokiNimi, size_t open, size_t nanpaSitelen) {
        assert(open <= pokiNimi.size() && "open alasa lon poki nimi li ken ala suli tawa suli pi poki ni");

        size_t nanpa = 0;

        for (; open < pokiNimi.size() && nanpaSitelen > 0; nanpaSitelen--) {
            size_t suliSitelen = static_cast<size_t>(std::abs(
                        UTF8LaSuliSitelen(pokiNimi, open)));

            if (suliSitelen == 0)
                suliSitelen = 1;

            open += suliSitelen;
            nanpa += suliSitelen;
        }

        return nanpa;
    }

    /*void ponaESitelenUTF8(std::string& pokiSitelen) {
        for (size_t i = 0; i < pokiSitelen.size();) {
            int suliSitelen = UTF8LaSuliSitelen(pokiSitelen, i);
            if (suliSitelen == 0) {
                suliSitelen = 1;
            } else
                suliSitelen = std::abs(suliSitelen);
            if (suliSitelen < 1) {
                if (suliSitelen == 0) {
                    pokiSitelen.erase(i, 1);

                } else
                    pokiSitelen.erase(i, suliSitelen * -1);

                pokiSitelen.insert(i, "\uFFFD");
                suliSitelen = 4;
            }

            i += suliSitelen;
        }
    }*/
}
