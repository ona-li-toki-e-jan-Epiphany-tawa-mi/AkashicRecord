#include "nimi_wawa.hpp"

#include <cassert>
#include <iostream>
#include <thread>
#include <chrono>
#include <cstring>
#include <optional>
#include <random>
#include <sstream>

#include "../../kepeken/ike.hpp"
#include "../../ante_toki/ante_toki.hpp"
#include "lawa.hpp"
#include "../../kepeken/lawa_OS.hpp"

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
 * @brief li toki e nimi pi nimi wawa lon pokiPali lon lupa pana.
 * @param lupaTawaToki li toki e nimi lon lupa ni.
 */
void tokiEAli(std::ostream& lupaTawaToki, std::stack<std::string>& pokiPali, unsigned int nanpaIjo) {
    for (; nanpaIjo > 0; nanpaIjo--) {
        lupaTawaToki << pokiPali.top();
        pokiPali.pop();
    }
}

/**
 * @brief nimi wawa li pakala ala e ijo ali ona la ni li wile kepeken tawa pakala e ijo awen.
 */
void pakalaEAwen(std::stack<std::string>& pokiPali, unsigned int nanpaIjo) {
    for (; nanpaIjo > 0; nanpaIjo--) pokiPali.pop();
}

/**
 * @brief li pilin e nanpa tan nanpaLili tawa nanpaSuli (lon [nanpaLili, nanpaSuli]).
 *
 * @tparam nanpa li wile sama int.
 * @param  nanpaLili li ken pilin e nanpa ni anu tan nanpa ni.
 * @param  nanpaSuli li ken pilin e nanpa ni anu tawa nanpa ni.
 * @return nanpa tan nanpaLili tawa nanpaSuli.
 */
template<typename nanpa>
nanpa pilinENanpa(nanpa nanpaLili, nanpa nanpaSuli) {
    static std::random_device         iloPiPaliKenPiSonaPona;
    static std::default_random_engine iloPiPaliKen(iloPiPaliKenPiSonaPona());

    std::uniform_int_distribution<nanpa> iloPiNanpaKen(nanpaLili, nanpaSuli);

    return iloPiNanpaKen(iloPiPaliKen);
}



/**
 * @brief toki([nimi...]) -> ala
 *
 * li toki e nimi lon lupa Stdout.
 */
void toki(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    tokiEAli(std::cout, sonaLawa.pokiPali, nanpaIjo);
    sonaLawa.pokiPali.push("");
}

/**
 * @brief tokiELinja([nimi...]) -> ala
 *
 * lon lupa Stdout la li toki e nimi e ike.
 */
void tokiELinja(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    toki(sonaLawa, nanpaIjo);
    std::cout << '\n';
}

/**
 * @brief tokiEIke([nimi...]) -> ala
 *
 * li toki e nimi lon lupa Stderr.
 */
void tokiEIke(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    tokiEAli(std::cerr, sonaLawa.pokiPali, nanpaIjo);
    sonaLawa.pokiPali.push("");
}

/**
 * @brief tokiEIkeELinja([nimi...]) -> ala
 *
 * lon lupa Stderr la li toki e nimi e ike.
 */
void tokiEIkeELinja(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    tokiEIke(sonaLawa, nanpaIjo);
    std::cerr << '\n';
}

/**
 * @brief kamaJo([nimi...]) -> nimi tan jan
 *
 * li toki e nimi tawa jan li kama jo e nimi tan ona.
 *
 * @throws std::runtime_error li kama jo e sitelen pi pini lipu.
 */
void kamaJo(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) noexcept(false) {
    // nimi li wile lon sewi pi poki pali tawa poki e nimi tawa jan. toki la 1 li lon tawa ni. ni ala la
    //      mi wile pana e poki nimi ala lon ona.
    if (nanpaIjo != 0) {
        toki(sonaLawa, nanpaIjo);

    } else
        sonaLawa.pokiPali.push("");

    if (!std::getline(std::cin, sonaLawa.pokiPali.top()))
        ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                                , sonaLawa.lonPiKasiPiTenpoNi
                                , "kamaJo(): " + ante_toki::nimiTawaJan("ike.lawa.pini_lipu")});
}

/**
 * @brief alaEIloPana() -> ala
 * @attention li lon taso lawa Windows en lawa UNIX.
 *
 * li ala (weka e sitelen) e ilo pana.
 */
void alaEIloPana(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    if (!kepeken::alaEIloPana())
        ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                                , sonaLawa.lonPiKasiPiTenpoNi
                                , ante_toki::nimiTawaJan("ike.lawa.ala_e_ilo_pana.li_alasa_ala")});

    sonaLawa.pokiPali.push("");
}

/**
 * @brief wan(nimi nimi [nimi...]) -> nimi wan tan nimi pana
 *
 * li wan e poki nimi mute.
 */
void wan(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    std::string nimiSin;

    for (; nanpaIjo > 0; nanpaIjo--) {
        nimiSin.append(std::move(sonaLawa.pokiPali.top()));
        sonaLawa.pokiPali.pop();
    }

    sonaLawa.pokiPali.push(std::move(nimiSin));
}

/**
 * @brief wanKepeken(nimiKepeken nimi nimi [nimi...]) -> nimi wan tan nimi pana kepeken nimi.
 *
 * li wan e nimi pana kepeken nimi kepeken pana. pana la wanKepeken(", " "1" "2" "3") -> "1, 2, 3".
 */
void wanKepeken(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    std::string nimiKepeken = std::move(sonaLawa.pokiPali.top());
    sonaLawa.pokiPali.pop(); nanpaIjo--;

    std::string nimiWan;

    for (; nanpaIjo > 0; nanpaIjo--) {
        nimiWan.append(std::move(sonaLawa.pokiPali.top()));
        sonaLawa.pokiPali.pop();

        if (nanpaIjo != 1)
            nimiWan.append(nimiKepeken);
    }

    sonaLawa.pokiPali.push(std::move(nimiWan));
}

/**
 * @brief awen(tenpo [tenpo...]) -> ala
 *
 * li awen lon tenpo pana.
 *
 * @throws std::runtime_error nanpa li nanpa ala anu nanpa ike.
 */
void awen(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) noexcept(false) {
    // li weka e ijo tan lupa tawa ni: sitelen ali lon ona li kama lon ilo CLI pi pana sitelen.
    std::cout.flush();
    std::cerr.flush();


    int tenpoLape = 0;
    bool nanpaIke = false;

    for (; nanpaIjo > 0; nanpaIjo--) {
        const std::string& ijo = sonaLawa.pokiPali.top();

        try {
            // nimi li jo e ala la li pona. ni la mi wile ala toki e ike.
            if (!ijo.empty())
                tenpoLape += std::stoi(ijo);

        } catch (const std::invalid_argument& liNanpaAla) {
            ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                                    , sonaLawa.lonPiKasiPiTenpoNi
                                    , "awen(): " + ante_toki::anteENimi(
                                        ante_toki::nimiTawaJan("ike.lawa.awen.nanpa_ala")
                                    , "%s", ijo)});

        } catch (const std::out_of_range& liNanpaIke) {
            ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                                    , sonaLawa.lonPiKasiPiTenpoNi
                                    , "awen(): " + ante_toki::anteENimi(
                                        ante_toki::nimiTawaJan("ike.lawa.awen.nanpa_ike")
                                    , "%s", ijo)});
        }

        sonaLawa.pokiPali.pop();
    }


    std::this_thread::sleep_for(std::chrono::milliseconds(tenpoLape));
    sonaLawa.pokiPali.push("");
}

/**
 * @brief pilin(nimi nimi \[nimi...\]) -> nimi wan tan nimi pana.
 *
 * li pilin e nimi tan nimi ken.
 */
void pilin(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    auto nanpaPilin = pilinENanpa<unsigned int>(0, nanpaIjo - 1);

    for (; nanpaPilin > 0; nanpaPilin--, nanpaIjo--)
        sonaLawa.pokiPali.pop();

    std::string pilin = std::move(sonaLawa.pokiPali.top());
    pakalaEAwen(sonaLawa.pokiPali, nanpaIjo);
    sonaLawa.pokiPali.push(std::move(pilin));
}

/**
 * @brief pokiPiLawaOS(nimiKen [nimiKen...]) -> nimi lon poki | ala
 *
 * li kama jo e poki pi lawa OS tan lawa OS sama "LANG" anu "USER" anu "LOGNAME".
 */
void pokiPiLawaOS(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    const char* pokiNanpaPiLawaOS = nullptr;

    // li kama jo e poki pi lawa OS pi nanpa 1 pi jo e ijo.
    for (; nanpaIjo > 0; nanpaIjo--) {
        pokiNanpaPiLawaOS = getenv(sonaLawa.pokiPali.top().c_str());

        if (pokiNanpaPiLawaOS != nullptr && strcmp(pokiNanpaPiLawaOS, "") != 0)
            break;

        sonaLawa.pokiPali.pop();
    }

    pakalaEAwen(sonaLawa.pokiPali, nanpaIjo);
    sonaLawa.pokiPali.push(pokiNanpaPiLawaOS != nullptr ? pokiNanpaPiLawaOS : "");
}

/**
 * @brief lawa([linja...]) -> ala
 *
 * li pali e lipu wawa tan linja li lawa e ilo nanpa kepeken ona.
 */
void lawa(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    std::string nimiPiLonLipu = sonaLawa.lonLipu + "(" +
                                std::to_string(sonaLawa.lonPiKasiPiTenpoNi->nanpaLinja) + "," +
                                std::to_string(sonaLawa.lonPiKasiPiTenpoNi->nanpaSitelenLonLinja) + "):"
                                " lawa()";
    ilo::KasiOpen lipuPali;

    {
        std::list<ilo::Ijo> lipuKipisi;

        {
            std::stringstream lipuWawaSin;

            for (; nanpaIjo > 0; nanpaIjo--) {
                lipuWawaSin << sonaLawa.pokiPali.top() << '\n';
                sonaLawa.pokiPali.pop();
            }

            lipuKipisi = ilo::kipisi(lipuWawaSin, nimiPiLonLipu);
        }

        lipuPali = ilo::pali(lipuKipisi, nimiPiLonLipu);
    }

    ilo::lawaELipu(lipuPali, sonaLawa.pokiAli, nimiPiLonLipu);
    sonaLawa.pokiPali.push("");
}

/**
 * @brief lawaELipu(lonLipu [lonLipu...]) -> ala
 *
 * li lawa e lipu lon lon pana. poki lon ona li kama lon lipu wawa sina. poki li lon lipu sina en
 * lipu pana la poki lon lipu pana li awen.
 */
void llawaELipu(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    for (; nanpaIjo > 0; nanpaIjo--) {
        std::string nimiLipu = std::move(sonaLawa.pokiPali.top());
        sonaLawa.pokiPali.pop();
        ilo::KasiOpen lipuPali;

        try {
            std::list<ilo::Ijo> lipuKipisi = ilo::kipisiELipu(nimiLipu);
            lipuPali = ilo::pali(lipuKipisi, nimiLipu);

        } catch (const std::invalid_argument& liOpenAla) {
            throw std::runtime_error(liOpenAla.what());
        }

        std::unordered_map<std::string, std::string> pokiAliSin;
        ilo::lawaELipu(lipuPali, pokiAliSin, nimiLipu);

        sonaLawa.pokiAli.swap(pokiAliSin);
        sonaLawa.pokiAli.merge(pokiAliSin);
    }

    sonaLawa.pokiPali.push("");
}

/**
 * @brief ikeLaTawaAla() -> ala
 *
 * lon tawa tawa lon tenpo ike li lon (kepeken ikeLaTawa()) la ni li weka e ona.
 */
void ikeLaTawaAla(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    sonaLawa.ikeLaLonTawaTawa = static_cast<size_t>(-1);
    sonaLawa.pokiPali.push("");
}

/**
 * @brief ike([nimi...]) -> ala
 *
 * li ike. nimi li nimi lon ike li pana tawa jan.
 */
void ike(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo) {
    std::string nimiIke;

    for (; nanpaIjo > 0; nanpaIjo--) {
        nimiIke.append(sonaLawa.pokiPali.top());
        sonaLawa.pokiPali.pop();
    }

    ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                            , sonaLawa.lonPiKasiPiTenpoNi
                            , nimiIke});
}



/**
 * @brief tawa(nimiTawa) -> ala
 *
 * li tawa lon ante.
 */
void tawa(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo, size_t lonTawaTawa) {
    sonaLawa.kasiPiTenpoNi = lonTawaTawa;
    sonaLawa.pokiPali.push("");
}

/**
 * @brief niLaTawa(nimiTawa nimiLon nimiPiLonAla [nimiTawaToki...]) -> nimi tan jan
 *
 * jan li toki e nimi lon la li tawa lon ante. ona li toki e nimi pi lon ala la li tawa ala. ona li toki
 *      e ante la li toki sin e toki seme tawa ona. nimi lon anu nimi pi lon ante li ken ala. ni la ona
 *      li sama nimi ali pi nimi ante ala pi lon anu lon ala.
 *
 * @throws std::runtime_error nimi lon en nimi pi lon ala li sama.
 * @throws std::runtime_error li kama jo e sitelen pi pini lipu.
 */
void niLaTawa(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo, size_t lonTawaTawa)
        noexcept(false) {
    std::string nimiLon = std::move(sonaLawa.pokiPali.top());
    sonaLawa.pokiPali.pop();
    std::string nimiPiLonAla = std::move(sonaLawa.pokiPali.top());
    sonaLawa.pokiPali.pop();
    nanpaIjo -= 2;

    if (nimiLon == nimiPiLonAla) {
        ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                                , sonaLawa.lonPiKasiPiTenpoNi
                                , "niLaTawa(): " + ante_toki::anteENimi( ante_toki::nimiTawaJan(
                                    "ike.lawa.tawa_ken.nimi_pi_lon_en_lon_ala_li_ken_ala_sama")
                                , "%s", nimiLon)});
    }

    std::list<std::string> nimiTawaJan;
    for (; nanpaIjo > 0; nanpaIjo--) {
        nimiTawaJan.push_back(std::move(sonaLawa.pokiPali.top()));
        sonaLawa.pokiPali.pop();
    }


    sonaLawa.pokiPali.push("");
    std::string& nimiJan = sonaLawa.pokiPali.top();

    // jan li toki e ike la li wile toki sin e ijo.
    while (true) {
        for (const auto& nimi : nimiTawaJan)
            std::cout << nimi;

        if (!std::getline(std::cin, nimiJan)) {
            ilo::paliEIke(sonaLawa, { sonaLawa.lonLipu
                                    , sonaLawa.lonPiKasiPiTenpoNi
                                    , "niLaTawa(): " + ante_toki::nimiTawaJan("ike.lawa.pini_lipu")});
        }


        if (nimiLon.empty() && nimiJan != nimiPiLonAla) {
            sonaLawa.kasiPiTenpoNi = lonTawaTawa;
            return;

        } else if (nimiPiLonAla.empty() && nimiJan != nimiLon) {
            return;

        } else if (nimiJan == nimiLon) {
            sonaLawa.kasiPiTenpoNi = lonTawaTawa;
            return;

        } else if (nimiJan == nimiPiLonAla)
            return;
    }
}

/**
 * @brief alaLaTawa(nimiTawa nimi [nimi...]) -> ala
 *
 * ijo ali pana li ala la li tawa.
 */
void alaLaTawa(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo, size_t lonTawaTawa) {
    for (; nanpaIjo > 0; nanpaIjo--) {
        if (!sonaLawa.pokiPali.top().empty()) {
            pakalaEAwen(sonaLawa.pokiPali, nanpaIjo);

            sonaLawa.pokiPali.push("");
            return;
        }

        sonaLawa.pokiPali.pop();
    }

    sonaLawa.kasiPiTenpoNi = lonTawaTawa;
    sonaLawa.pokiPali.push("");
}

/**
 * @brief ikeLaTawa(nimiTawa) -> ala
 *
 * ike li kama la ilo lawa li tawa lon pana.
 */
void ikeLaTawa(ilo::SonaLawa& sonaLawa, unsigned int nanpaIjo, size_t lonTawaTawa) {
    sonaLawa.ikeLaLonTawaTawa = lonTawaTawa;
    sonaLawa.pokiPali.push("");
}



namespace ilo {
    const unsigned int nanpaPiIjoWileAli = -1;



    TomoPiNimiWawa::TomoPiNimiWawa(unsigned int nanpaLiliPiIjoWile, unsigned int nanpaSuliPiIjoWile)
            : nanpaLiliPiIjoWile(nanpaLiliPiIjoWile), nanpaSuliPiIjoWile(nanpaSuliPiIjoWile) {
        assert( (nanpaLiliPiIjoWile == nanpaPiIjoWileAli || nanpaSuliPiIjoWile == nanpaPiIjoWileAli
             || nanpaSuliPiIjoWile >= nanpaLiliPiIjoWile)
             && "nanpa lon nanpaLiliPiIjoWile li ken ala suli tawa nanpa lon nanpaSuliPiIjoWile!");
    }

    TomoPiNimiWawa::~TomoPiNimiWawa() {}


    /**
     * @brief li kama sona e ni: nanpa ijo en ijo ante li ike tawa lawa kepeken nimi wawa. ni la li
     *      "throw" e ike.
     * @param tomoPiNimiWawa nimi wawa tawa kama sona.
     */
    void ikeLaIke( SonaLawa& sonaLawa
                 , unsigned int nanpaIjo
                 , const TomoPiNimiWawa& tomoPiNimiWawa) {
        assert( (tomoPiNimiWawa.nanpaLiliPiIjoWile == nanpaPiIjoWileAli || nanpaIjo >= tomoPiNimiWawa.nanpaLiliPiIjoWile)
             && (tomoPiNimiWawa.nanpaSuliPiIjoWile == nanpaPiIjoWileAli || nanpaIjo <= tomoPiNimiWawa.nanpaSuliPiIjoWile)
             && "nanpa ijo tawa nimi wawa li ken ala lon ante tannanpaLiliPiIjoWile en nanpaSuliPiIjoWile!");

        assert(sonaLawa.pokiPali.size() >= nanpaIjo && "poki pali li jo e ijo lili lili tawa lawa e nimi wawa!");
    }


    NimiWawa::NimiWawa(NimiWawaKiwen nimiWawaKiwen)
            : NimiWawa(nanpaPiIjoWileAli, nimiWawaKiwen, nanpaPiIjoWileAli) {}

    NimiWawa::NimiWawa(unsigned int nanpaLiliPiIjoWile, NimiWawaKiwen nimiWawaKiwen)
            : NimiWawa(nanpaLiliPiIjoWile, nimiWawaKiwen, nanpaPiIjoWileAli) {}

    NimiWawa::NimiWawa(NimiWawaKiwen nimiWawaKiwen, unsigned int nanpaSuliPiIjoWile)
            : NimiWawa(nanpaPiIjoWileAli, nimiWawaKiwen, nanpaSuliPiIjoWile) {}

    NimiWawa::NimiWawa( unsigned int nanpaLiliPiIjoWile
                      , NimiWawaKiwen nimiWawaKiwen
                      , unsigned int nanpaSuliPiIjoWile)
            : TomoPiNimiWawa(nanpaLiliPiIjoWile, nanpaSuliPiIjoWile) {
        this->nimiWawaKiwen = nimiWawaKiwen;
    }

    void NimiWawa::lawa(SonaLawa& sonaLawa, unsigned int nanpaIjo) const noexcept(false) {
        ikeLaIke(sonaLawa, nanpaIjo, *this);
        this->nimiWawaKiwen(sonaLawa, nanpaIjo);
    }


    NimiWawaTawa::NimiWawaTawa(NimiWawaTawaKiwen nimiWawaTawaKiwen)
            : NimiWawaTawa(nanpaPiIjoWileAli, nimiWawaTawaKiwen, nanpaPiIjoWileAli) {}

    NimiWawaTawa::NimiWawaTawa(unsigned int nanpaLiliPiIjoWile, NimiWawaTawaKiwen nimiWawaTawaKiwen)
            : NimiWawaTawa(nanpaLiliPiIjoWile, nimiWawaTawaKiwen, nanpaPiIjoWileAli) {}

    NimiWawaTawa::NimiWawaTawa(NimiWawaTawaKiwen nimiWawaTawaKiwen, unsigned int nanpaSuliPiIjoWile)
            : NimiWawaTawa(nanpaPiIjoWileAli, nimiWawaTawaKiwen, nanpaSuliPiIjoWile) {}

    NimiWawaTawa::NimiWawaTawa( unsigned int nanpaLiliPiIjoWile
                              , NimiWawaTawaKiwen nimiWawaTawaKiwen
                              , unsigned int nanpaSuliPiIjoWile)
            : TomoPiNimiWawa(nanpaLiliPiIjoWile, nanpaSuliPiIjoWile) {
        this->nimiWawaTawaKiwen = nimiWawaTawaKiwen;
    }

    void NimiWawaTawa::lawa(SonaLawa& sonaLawa, unsigned int nanpaIjo, size_t lonTawaTawa) const
            noexcept(false) {
        ikeLaIke(sonaLawa, nanpaIjo, *this);
        this->nimiWawaTawaKiwen(sonaLawa, nanpaIjo, lonTawaTawa);
    }



    const std::unordered_map<std::string, NimiWawa> nimiTawaNimiWawa = {
        {"toki",           NimiWawa(&toki)},
        {"tokiELinja",     NimiWawa(&tokiELinja)},
        {"tokiEIke",       NimiWawa(&tokiEIke)},
        {"tokiEIkeELinja", NimiWawa(&tokiEIkeELinja)},
        {"alaEIloPana",    NimiWawa(&alaEIloPana, 0)},

        {"kamaJo",         NimiWawa(   &kamaJo)},
        {"wan",            NimiWawa(2, &wan)},
        {"wanKepeken",     NimiWawa(3, &wanKepeken)},
        {"pilin",          NimiWawa(2, &pilin)},
        {"awen",           NimiWawa(1, &awen)},

        {"pokiPiLawaOS",   NimiWawa(1, &pokiPiLawaOS)},

        {"lawa",           NimiWawa(   &lawa)},
        {"lawaELipu",      NimiWawa(1, &llawaELipu)},

        {"ikeLaTawaAla",   NimiWawa(&ikeLaTawaAla, 0)},
        {"ike",            NimiWawa(&ike)}
    };

    const std::unordered_map<std::string, NimiWawaTawa> nimiTawaNimiWawaTawa = {
        {"tawa",        NimiWawaTawa(   &tawa, 0)},
        {"niLaTawa",    NimiWawaTawa(2, &niLaTawa)},
        {"alaLaTawa",   NimiWawaTawa(1, &alaLaTawa)},

        {"ikeLaTawa",   NimiWawaTawa(&ikeLaTawa, 0)}
    };

    const std::string& tomoPiNimiWawaTawaNimi(const TomoPiNimiWawa* tomoPiNimiWawa) noexcept(false) {
        static std::optional<std::unordered_map<NimiWawaKiwen, std::string>> nimiWawaKiwenTawaNimi
                = std::nullopt;
        static std::optional<std::unordered_map<NimiWawaTawaKiwen, std::string>> nimiWawaTawaKiwenTawaNimi
                = std::nullopt;


        auto nimiWawa = dynamic_cast<const NimiWawa*>(tomoPiNimiWawa);

        if (nimiWawa != nullptr) {
            if (!nimiWawaKiwenTawaNimi.has_value()) {
                nimiWawaKiwenTawaNimi = std::unordered_map<NimiWawaKiwen, std::string>();

                for (const auto& [nimi, nimiWawa] : nimiTawaNimiWawa)
                    nimiWawaKiwenTawaNimi->operator[](nimiWawa.nimiWawaKiwen) = nimi;
            }

            return nimiWawaKiwenTawaNimi->at(nimiWawa->nimiWawaKiwen);
        }


        if (!nimiWawaTawaKiwenTawaNimi.has_value()) {
            nimiWawaTawaKiwenTawaNimi = std::unordered_map<NimiWawaTawaKiwen, std::string>();

            for (const auto& [nimi, nimiWawa] : nimiTawaNimiWawaTawa)
                nimiWawaTawaKiwenTawaNimi->operator[](nimiWawa.nimiWawaTawaKiwen) = nimi;
        }

        return nimiWawaTawaKiwenTawaNimi->at(static_cast<const NimiWawaTawa*>(tomoPiNimiWawa)
                ->nimiWawaTawaKiwen);
    }
}
