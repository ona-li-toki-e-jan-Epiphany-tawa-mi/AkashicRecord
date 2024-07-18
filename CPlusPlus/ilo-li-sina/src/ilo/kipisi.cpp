#include "kipisi.hpp"

#include <regex>
#include <iostream>
#include <fstream>
#include <optional>
#include <cassert>

#include "../kepeken/ike.hpp"
#include "../kepeken/poki_Map.hxx"
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

namespace ilo {
    Ijo::Ijo(NimiIjo nimiIjo, size_t linja, size_t sitelenLonLinja)
        : Ijo::Ijo(nimiIjo, "", linja, sitelenLonLinja) {}

    Ijo::Ijo(NimiIjo&& nimiIjo, std::string&& ijo, size_t&& linja, size_t&& sitelenLonLinja) {
        this->nimiIjo = nimiIjo;
        this->ijo.swap(ijo);
        this->lonIjo = {linja, sitelenLonLinja};
    }

    Ijo::Ijo(NimiIjo nimiIjo, const std::string& ijo, size_t linja, size_t sitelenLonLinja) {
        this->nimiIjo = nimiIjo;
        this->ijo = ijo;
        this->lonIjo = {linja, sitelenLonLinja};
    }



    const std::regex SITELEN_PI_POKI_NANPA("[a-zA-Z0-9_]", std::regex_constants::optimize);
    const std::regex SITELEN_PI_LUKIN_ALA ("\\s",          std::regex_constants::optimize);
    // jan li sitelen e sitelen lon poka pilin la ni li pali e sitelen lon poka pi pilin ala.
    const std::unordered_map<char, char> nimiTawaSitelenNasa = {
        {'n', '\n'}, {'t', '\t'}, {'b', '\b'}, {'v', '\v'}, {'"', '"'}, {'\'', '\''}, {'`', '`'},
        {'\\', '\\'}};

    /**
     * @brief li pona e pana ijo tawa nimi wawa kipisi.
     */
    struct SonaKipisi {
        const std::string& lonLipu;

        std::list<Ijo>&              pokiIjo;
        std::string&                 linja;
        size_t&                      nanpaLinja;
        std::string::const_iterator& alasaSitelen;
    };

    /**
     * @brief li kama jo e suli tan open linja tawa sitelen lon alasaSitelen.
     *
     * @param  linja li kama jo tan open pi linja ni.
     * @param  alasaSitelen li kama jo e lon pi sitelen lon ni.
     * @return lon sitelen.
     */
    size_t kamaJoELonSitelen(const std::string& linja, std::string::const_iterator alasaSitelen) {
        // nanpa ni li tawa jan. ni li open tan 0, taso, ni li pona ala tawa jan. tan ni la mi li en
        //      e 1.
        return static_cast<size_t>(std::distance(linja.cbegin(), alasaSitelen)) + 1;
    }




    /**
     * @brief li pali e nimi tawa tawa. (ijo sama ni: Test:)
     * @return li pali pona la true. ike li lon la false.
     */
    bool paliENimiTawaTawa(SonaKipisi& sonaKipisi) {
        bool liPona = true;
        bool liPaliELonTawaTawa = false;

        if (sonaKipisi.pokiIjo.size() >= 1) {
            auto ijoLonMonsi = sonaKipisi.pokiIjo.end();
            ijoLonMonsi--;

            if (ijoLonMonsi->nimiIjo == NimiIjo::POKI) {
                ijoLonMonsi->nimiIjo = NimiIjo::NIMI_TAWA_TAWA;
                liPaliELonTawaTawa = true;
            }
        }

        if (!liPaliELonTawaTawa) {
            kepeken::tokiEIke({ sonaKipisi.lonLipu
                              , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                        , sonaKipisi.alasaSitelen)
                              , ante_toki::nimiTawaJan("ike.kipisi.nimi_tawa.nimi_li_wile")});
            liPona = false;
        }

        if (sonaKipisi.pokiIjo.size() >= 2) {
            auto ijoLonMonsi = sonaKipisi.pokiIjo.cend();
            for (int i = 0; i < 2; i++) ijoLonMonsi--;

            if (ijoLonMonsi->nimiIjo != NimiIjo::LINJA_SIN) {
                kepeken::tokiEIke({ sonaKipisi.lonLipu
                                  , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                            , sonaKipisi.alasaSitelen)
                                  , ante_toki::nimiTawaJan("ike.kipisi.nimi_tawa.li_ken_lon_open_linja_taso")});
                liPona = false;
            }
        }

        return liPona;
    }

    /**
     * @brief li pali e poki nimi. (ijo sama ni: "test")
     * @return li pali pona la true. ike li lon la false.
     */
    bool paliEPokiNimi(SonaKipisi& sonaKipisi) {
        bool liPona = true;

        std::string pokiNimi;
        bool liJoEPini         = false;
        const auto openPoki    = sonaKipisi.alasaSitelen;
        const char sitelenPoki = *sonaKipisi.alasaSitelen;

        sonaKipisi.alasaSitelen++;
        for (; sonaKipisi.alasaSitelen < sonaKipisi.linja.cend(); sonaKipisi.alasaSitelen++) {
            // pini pi poki nimi.
            if (*sonaKipisi.alasaSitelen == sitelenPoki) {
                sonaKipisi.alasaSitelen++;
                liJoEPini = true;
                goto liPiniPiPokiSitelen; // sama "break".
            }

            switch (*sonaKipisi.alasaSitelen) {
                // nimi pi sitelen nasa (sama \n anu \v).
                case '\\':
                    sonaKipisi.alasaSitelen++;
                    if (sonaKipisi.alasaSitelen == sonaKipisi.linja.cend())
                        goto liPiniPiPokiSitelen;

                    try {
                        pokiNimi.push_back(nimiTawaSitelenNasa.at(*sonaKipisi.alasaSitelen));

                    } catch (const std::out_of_range& liSitelenAlaTawaSitelenNasa) {
                        kepeken::tokiEIke({ sonaKipisi.lonLipu
                                          , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                                    , sonaKipisi.alasaSitelen)
                                          , ante_toki::anteENimi( ante_toki::nimiTawaJan("ike.kipisi.poki_nimi.nimi_pi_sitelen_nasa_pi_sona_ala")
                                                                , "%s", std::string(1, *sonaKipisi.alasaSitelen))});
                        liPona = false;
                    }

                    continue;

                default:
                    pokiNimi.push_back(*sonaKipisi.alasaSitelen);
            }
        }
    liPiniPiPokiSitelen:
        sonaKipisi.alasaSitelen--;

        if (!liJoEPini) {
            kepeken::tokiEIke({ sonaKipisi.lonLipu
                              , sonaKipisi.nanpaLinja, sonaKipisi.linja.size() + 1
                              , ante_toki::nimiTawaJan("ike.kipisi.poki_nimi.li_jo_ala_e_pini")});
            liPona = false;
        }

        sonaKipisi.pokiIjo.emplace_back( NimiIjo::POKI_NIMI
                                       , std::move(pokiNimi)
                                       , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                                 , sonaKipisi.alasaSitelen));
        return liPona;
    }

    /**
     * @brief li pali e ijo pi nimi wawa. (ijo sama ni: () )
     * @return li pali pona la true. ike li lon la false.
     */
    bool paliEPokiPiIjoPiNimiWawa(SonaKipisi& sonaKipisi) {
        sonaKipisi.pokiIjo.emplace_back( NimiIjo::POKI_PI_IJO_PI_NIMI_WAWA
                                       , std::string(1, *sonaKipisi.alasaSitelen)
                                       , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                                 , sonaKipisi.alasaSitelen));

        if (*sonaKipisi.alasaSitelen == '(') {
            bool liPaliENimiWawa = false;

            if (sonaKipisi.pokiIjo.size() > 1) {
                auto ijoLonMonsi = sonaKipisi.pokiIjo.end();
                for (int i = 0; i < 2; i++) ijoLonMonsi--;

                if (ijoLonMonsi->nimiIjo == NimiIjo::POKI) {
                    ijoLonMonsi->nimiIjo = NimiIjo::NIMI_WAWA;
                    liPaliENimiWawa = true;
                }
            }

            if (!liPaliENimiWawa) {
                kepeken::tokiEIke({ sonaKipisi.lonLipu
                                  , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                            , sonaKipisi.alasaSitelen)
                                  , ante_toki::nimiTawaJan("ike.kipisi.nimi_wawa.nimi_li_wile")});
                return false;
            }
        }

        return true;
    }

    /**
     * @brief li pali e poki. (ijo sama ni: test)
     */
    void paliEPoki(SonaKipisi& sonaKipisi) {
        std::string nimiPiPokiNanpa(1, *sonaKipisi.alasaSitelen);
        const auto openPiPokiNanpa = sonaKipisi.alasaSitelen;

        sonaKipisi.alasaSitelen++;
        for (; sonaKipisi.alasaSitelen < sonaKipisi.linja.cend(); sonaKipisi.alasaSitelen++) {
            if ((*sonaKipisi.alasaSitelen & 0b1'0000000) == 0
                    && !std::regex_match( sonaKipisi.alasaSitelen
                                        , sonaKipisi.alasaSitelen + 1
                                        , SITELEN_PI_POKI_NANPA))
                break;

            nimiPiPokiNanpa.push_back(*sonaKipisi.alasaSitelen);
        }
        sonaKipisi.alasaSitelen--;

        sonaKipisi.pokiIjo.emplace_back( NimiIjo::POKI
                                       , std::move(nimiPiPokiNanpa)
                                       , sonaKipisi.nanpaLinja, kamaJoELonSitelen( sonaKipisi.linja
                                                                                 , openPiPokiNanpa));
    }



    std::list<Ijo> kipisi(std::istream& lipu, const std::string& lonLipu) noexcept(false) {
        std::list<Ijo> pokiIjo;

        std::string linja;
        size_t      nanpaLinja   = 1;
        auto        alasaSitelen = linja.cbegin();
        SonaKipisi  sonaKipisi   = {lonLipu, pokiIjo, linja, nanpaLinja, alasaSitelen};

        bool liLipuPona = true;

        // ona li lon la li weka e sitelen BOM.
        std::getline(lipu, linja);
        if (linja.compare(0, 3, "\xEF\xBB\xBF") == 0)
            linja.erase(0, 3);

        do {
            if (linja.size() != 0)
                for (alasaSitelen = linja.cbegin(); alasaSitelen < linja.cend(); alasaSitelen++) {
                    sonaKipisi.alasaSitelen = alasaSitelen;

                    switch (*alasaSitelen) {
                        case '=': {
                            pokiIjo.emplace_back( NimiIjo::PANA_LON_POKI
                                                , sonaKipisi.nanpaLinja
                                                , kamaJoELonSitelen( sonaKipisi.linja
                                                                   , sonaKipisi.alasaSitelen));
                            break;
                        }

                        case ':': {
                            liLipuPona = paliENimiTawaTawa(sonaKipisi) && liLipuPona;
                            break;
                        }

                        case '"':
                        case '\'':
                        case '`': {
                            liLipuPona = paliEPokiNimi(sonaKipisi) && liLipuPona;
                            break;
                        }

                        // li ken e ni: jan li sitelen e nimi sona tawa toki e sona.
                        case '#':
                            goto liNimiSona;

                        // li ken e ni: jan li sitelen e ijo wan lon linja mute.
                        case '\\':
                            goto linjaSinLiWileAla;

                        case '(':
                        case ')': {
                            liLipuPona = paliEPokiPiIjoPiNimiWawa(sonaKipisi) && liLipuPona;
                            break;
                        }

                        default:
                            // sitelen li lon ala kulupu ASCII la ona li ken lon nimi poki. '& 0b1000_0000' li alasa e ona.
                            if ((*alasaSitelen & 0b10000000) != 0
                                    || std::regex_match(alasaSitelen, alasaSitelen+1, SITELEN_PI_POKI_NANPA)) {
                                paliEPoki(sonaKipisi);
                                break;
                            }

                            // li weka e sitelen pi lukin ala.
                            if (std::regex_match(alasaSitelen, alasaSitelen+1, SITELEN_PI_LUKIN_ALA))
                                break;



                            // sitelen li wile ala la li ike.
                            size_t lonSitelen  = kamaJoELonSitelen(linja, alasaSitelen);
                            size_t suliSitelen = std::abs(ante_toki::UTF8LaSuliSitelen(linja, lonSitelen - 1));
                            if (suliSitelen == 0)
                                suliSitelen = 1;
                            // sitelen li suli tawa 1 la mi wile tawa pini ona.
                            alasaSitelen += suliSitelen - 1;

                            kepeken::tokiEIke({ lonLipu
                                              , nanpaLinja, lonSitelen
                                              , ante_toki::anteENimi( ante_toki::nimiTawaJan("ike.sitelen_pi_wile_ala")
                                                                    , "%s", linja.substr(lonSitelen - 1, suliSitelen))});
                            liLipuPona = false;
                    }
                }
        liNimiSona:

            // linja sin mute lon poka ona li wile ala. 1 taso li wile.
            if (pokiIjo.back().nimiIjo != NimiIjo::LINJA_SIN)
                pokiIjo.emplace_back( NimiIjo::LINJA_SIN
                                    , nanpaLinja, linja.size() + 1);
        linjaSinLiWileAla:
            nanpaLinja++;

        } while (std::getline(lipu, linja));

        if (!liLipuPona)
            throw std::runtime_error("lipu wawa ike!");

        // tenpo ali la linja li wile jo e LINJA_SIN lon pini. ken la linja pini lon pokiIjo li jo ala
        //      e ni. ni li pona e ni.
        if (!pokiIjo.empty() && pokiIjo.back().nimiIjo != NimiIjo::LINJA_SIN)
            pokiIjo.emplace_back( NimiIjo::LINJA_SIN
                                , nanpaLinja, linja.size() + 1);


        return pokiIjo;
    }

    std::list<Ijo> kipisiELipu(const std::string& lonLipu) noexcept(false) {
        std::ifstream lipu(lonLipu);

        if (!lipu.is_open()) {
            kepeken::tokiEIke({
                ante_toki::anteENimi(
                    ante_toki::nimiTawaJan("ike.kipisi.li_ken_ala_open_e_lipu"),
                    "%s", lonLipu)});
            throw std::invalid_argument("li ken ala open e poki '" + lonLipu + "'");
        }

        return kipisi(lipu, lonLipu);
    }



    std::string nimiIjoTawaNimiPiNimiIjo(NimiIjo nimiIjo) {
        switch (nimiIjo) {
            case NimiIjo::POKI:
                return "toki.ijo_kipisi.poki";
            case NimiIjo::PANA_LON_POKI:
                return "toki.ijo_kipisi.pana_lon_poki";
            case NimiIjo::POKI_NIMI:
                return "toki.ijo_kipisi.poki_nimi";
            case NimiIjo::NIMI_WAWA:
                return "toki.ijo_kipisi.nimi_wawa";
            case NimiIjo::POKI_PI_IJO_PI_NIMI_WAWA:
                return "toki.ijo_kipisi.poki_pi_ijo_pi_nimi_wawa";
            case NimiIjo::LINJA_SIN:
                return "toki.ijo_kipisi.linja_sin";
            case NimiIjo::NIMI_TAWA_TAWA:
                return "toki.ijo_kipisi.nimi_tawa_tawa";

            default:
                assert(false && "li kama jo e nimi pi kulupu nimi pi sona ala");
                return "ERROR";
        }
    }

    const std::unordered_map<char, char>& sitelenNasaTawaNimi() {
        static std::optional<std::unordered_map<char, char>> sitelenNasaTawaNimi = std::nullopt;

        if (!sitelenNasaTawaNimi.has_value())
            sitelenNasaTawaNimi = std::optional(kepeken::pokiMAPLonNasinAnte(nimiTawaSitelenNasa));

        return *sitelenNasaTawaNimi;
    }
}
