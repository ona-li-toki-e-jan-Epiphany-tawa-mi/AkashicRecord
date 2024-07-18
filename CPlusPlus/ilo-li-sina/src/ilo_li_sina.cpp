#include <functional>
#include <cassert>

#include "ante_toki/ante_toki.hpp"
#include "kepeken/toki.hpp"
#include "kepeken/ike.hpp"
#include "ilo/toki.hpp"
#include "ilo/lawa/lawa.hpp"

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
 * @brief nimi pi ilo pilin ali. li tawa poki en kama jo.
 */
enum class NimiPiIloPilin {
    TOKI_E_IJO,
    TOKI_E_KASI,
    HELP,
    VERSION
};

/**
 * @brief ilo pilin tawa lawa e lipu ni tan ilo CLI pi pana nimi (sama -h anu --toki-e-kasi).
 */
struct IloPilin {
    // nimi lili pi ilo pilin. li ken \0 tawa ala.
    const char nimiLili = '\0';
    // nimi suli pi ilo pilin. li ken "" tawa ala.
    const std::string nimiSuli;
    // nimi pi kama sona kepeken pi ilo pilin.
    const std::string sonaKepeken;
    // jan li kepeken e ilo pilin ni.
    bool liLon = false;
};

/**
 * @brief ilo pilin ali pi lipu ni.
 *
 * ni li lon tan ni: pana e ilo pilin sin li ken lon tenpo lili.
 */
std::unordered_map<NimiPiIloPilin, IloPilin> iloPilinAli = {
    {NimiPiIloPilin::TOKI_E_IJO,  {'i',  "toki-e-ijo",  "ilo_CLI.ilo_pilin.toki_e_ijo",  false}},
    {NimiPiIloPilin::TOKI_E_KASI, {'k',  "toki-e-kasi", "ilo_CLI.ilo_pilin.toki_e_kasi", false}},
    {NimiPiIloPilin::HELP,		  {'h',  "help",        "ilo_CLI.ilo_pilin.help",		 false}},
    {NimiPiIloPilin::VERSION,     {'\0', "version",     "ilo_CLI.ilo_pilin.version",     false}}
};



/**
 * @brief li toki e sona kepeken pi ilo ni tawa jan.
 */
void tokiESonaKepeken() {
    // nimi lipu.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.nimi_lipu") << ":\n";
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , "ilo_li_sina - " + ante_toki::nimiTawaJan("ilo_CLI.nimi_lipu.nimi")
                                  , 8, 0);


    std::cout << '\n';


    // nasin kepeken.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.nasin_kepeken") << ":\n";
    std::string nimiPiNasinKepeken("ilo_li_sina ");

    if (!iloPilinAli.empty()) {
        nimiPiNasinKepeken.append("[-");
        for (const auto& [nimiPiIloPilin, iloPilin] : iloPilinAli)
            if (iloPilin.nimiLili != '\0')
                nimiPiNasinKepeken.append(1, iloPilin.nimiLili);
        nimiPiNasinKepeken.append("] ");
    }

    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , nimiPiNasinKepeken + ante_toki::nimiTawaJan("ilo_CLI.ilo_pilin.lipu_wawa") + "..."
                                  , 8, 8);


    std::cout << '\n';


    // sona.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.sona") << ":\n";
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.sona.nimi")
                                  , 8, 0);


    std::cout << '\n';


    // ilo pilin.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.ilo_pilin") << ":\n";

    if (!iloPilinAli.empty()) {
        for (const auto& [nimiPiIloPilin, iloPilin] : iloPilinAli) {
            std::string nimi;
            if (iloPilin.nimiLili != '\0')
                nimi.append(1, '-').append(1, iloPilin.nimiLili);
            if (iloPilin.nimiLili != '\0' && !iloPilin.nimiSuli.empty())
                nimi.append(", ");
            if (!iloPilin.nimiSuli.empty())
                nimi.append("--").append(iloPilin.nimiSuli);
            kepeken::tokiELinjaPiLukinPona( std::cout
                                          , nimi
                                          , 8, 8);

            kepeken::tokiELinjaPiLukinPona( std::cout
                                          , ante_toki::nimiTawaJan(iloPilin.sonaKepeken)
                                          , 16, 0);
            std::cout << '\n';
        }
    }

    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.ilo_pilin.lipu_wawa")
                                  , 8, 8);
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.ilo_pilin.lipu_wawa.sona")
                                  , 16, 0);


    std::cout << '\n';


    // jan pali.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.jan_pali") << ":\n";
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , "ona li toki e jan Epiphany tawa mi."
                                  , 8, 0);


    std::cout << '\n';


    // ike.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.ike") << ":\n";
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.ike.ma_pi_toki_ike")
                                  , 8, 0);


    std::cout << '\n';


    // jo lipu.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.jo_lipu") << ":\n";
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.jo_lipu.nimi")
                                  , 8, 0);


    std::cout << '\n';


    // ijo sin.
    std::cout << ante_toki::nimiTawaJan("ilo_CLI.ijo_sin") << ":\n";
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.ijo_sin.nimi_wawa")
                                  , 8, 8);
    kepeken::tokiELinjaPiLukinPona( std::cout
                                  , ante_toki::nimiTawaJan("ilo_CLI.ijo_sin.nasin_kepeken")
                                  , 8, 8);
}

/**
 * @brief li toki e nanpa pi ilo ni tawa jan.
 */
void tokiENanpaPiIloNi() {
    std::cout << "ilo_li_sina " << NANPA_PI_ILO_PI_ILO_LI_SINA << '\n';
}

/**
 * @brief li toki e ijo tan kipisi pi lipu wawa.
 * @param lonLipu lon pi lipu wawa.
 */
void tokiELipuKipisi(const std::string& lonLipu) {
    ilo::tokiELipuKipisi(ilo::kipisiELipu(lonLipu), lonLipu);
}

/**
 * @brief li toki e kasi tan pali pi lipu wawa.
 * @param lonLipu lon pi lipu wawa.
 */
void tokiEKasiLipu(const std::string& lonLipu) {
    ilo::KasiOpen kasiOpen;

    {
        std::list<ilo::Ijo> lipuKipisi = ilo::kipisiELipu(lonLipu);
        kasiOpen                       = ilo::pali(lipuKipisi, lonLipu);
    }

    ilo::tokiELipuPali(kasiOpen, lonLipu);
}

/**
 * @brief li lawa e ilo nanpa kepeken lipu wawa pana.
 * @param lonLipu lon pi lipu wawa.
 */
void lawaEIloNanpa(const std::string& lonLipu) {
    ilo::KasiOpen kasiOpen;

    {
        std::list<ilo::Ijo> lipuKipisi = ilo::kipisiELipu(lonLipu);
        kasiOpen                       = ilo::pali(lipuKipisi, lonLipu);
    }

    std::unordered_map<std::string, std::string> pokiAli;
    ilo::panaEPokiOpenLonPokiAli(pokiAli, lonLipu);
    ilo::lawaELipu(kasiOpen, pokiAli, lonLipu);
}



/**
 * ilo tawa lawa e ilo nanpa kepeken toki "ilo li sina". toki "ilo li sina" li toki pi lawa e ilo
 * nanpa li kepeken e nimi wawa GOTO li lawa e nimi wawa lon nasin pona li kepeken e jan tawa nanpa
 * e nanpa li kepeken taso e poki pi sitelen nimi li tawa ilo nanpa pi wile pi pali ala en awen lon
 * tomo. ;)
 *
 * @author ona li toki e jan Epiphany tawa mi.
 *
 * @see <https://github.com/ona-li-toki-e-jan-Epiphany-tawa-mi/ilo-li-sina> poki pi lipu wawa ni lon
 *      lipu KiApu.
 */
int main(const int nanpaPiNimiPilin, const char *const *const nimiPilin) {
    const std::vector<std::string> iloPilinPana(nimiPilin + 1, nimiPilin + nanpaPiNimiPilin);
    const std::string nimiPiILO_LI_SINA(nimiPilin[0]);

    // kama jo e toki wile li wile lon monsi pi toki e ijo.
    ante_toki::alasaETokiWile();
    // li wile pana e nimi pi ilo Ilo Li Sina tawa ike.hpp tawa toki ike.
    kepeken::anteENimiPiILO_LI_SINA(nimiPiILO_LI_SINA);


    assert([&]() {
        for (auto& [nimi, iloPilin] : iloPilinAli)
            if (iloPilin.nimiLili == '\0' && iloPilin.nimiSuli.empty())
                return false;
        return true;

    }() && "ilo pilin la nimi lili en nimi suli li ken ala ala lon tenpo sama");

    std::list<const std::string*> lipuWawaPana;

    for (auto alasaIloPilin = iloPilinPana.cbegin(); alasaIloPilin < iloPilinPana.cend(); alasaIloPilin++)
        if (alasaIloPilin->size() > 2 && alasaIloPilin->compare(0, 2, "--") == 0) {
            bool liLon = false;

            for (auto& [nimi, iloPilin] : iloPilinAli)
                // nimi suli li ala la ona li lon ala.
                if (!iloPilin.nimiSuli.empty() && alasaIloPilin->compare(2, std::string::npos, iloPilin.nimiSuli) == 0) {
                    iloPilin.liLon = true;
                    liLon = true;
                    break;
                }

            if (!liLon) {
                kepeken::tokiEIke({
                    ante_toki::anteENimi( ante_toki::nimiTawaJan("ike.ilo_CLI.ilo_pilin.ilo_pi_sona_ala")
                                        , "%s", *alasaIloPilin)});
                std::cout << ante_toki::nimiTawaJan("ike.ilo_CLI.o_lukin_e_sona_kepeken") << '\n';

                return 1;
            }

        } else if (alasaIloPilin->size() > 1 && alasaIloPilin->at(0) == '-') {
            for (size_t alasaNimiLili = 1; alasaNimiLili < alasaIloPilin->size(); alasaNimiLili++) {
                bool liLon = false;

                for (auto& [nimi, iloPilin] : iloPilinAli)
                    // nimi lili li \0 la ona li lon ala.
                    if (iloPilin.nimiLili != '\0' && alasaIloPilin->at(alasaNimiLili) == iloPilin.nimiLili) {
                        iloPilin.liLon = true;
                        liLon = true;

                        break;
                    }

                if (!liLon) {
                    // li kama jo e suli pi sitelen e ilo pilin tan ni: ni la li ken toki pona e sitelen UTF-8 (sama п anu 草.)
                    size_t suliPiIloPilin = ante_toki::UTF8LaSuliSitelen(*alasaIloPilin, alasaNimiLili);

                    kepeken::tokiEIke({
                        ante_toki::anteENimi( ante_toki::nimiTawaJan("ike.ilo_CLI.ilo_pilin.ilo_pi_sona_ala")
                                            , "%s", std::string("-") + alasaIloPilin->substr( alasaNimiLili
                                                                                            , suliPiIloPilin))});
                    std::cout << ante_toki::nimiTawaJan("ike.ilo_CLI.o_lukin_e_sona_kepeken") << '\n';

                    return 1;
                }
            }

        } else
            // nimi li ilo pilin ala la ona li nimi pi lipu wawa.
            lipuWawaPana.push_back(&(*alasaIloPilin));


    if (iloPilinAli[NimiPiIloPilin::HELP].liLon) {
        tokiESonaKepeken();
        return 0;

    } else if (iloPilinAli[NimiPiIloPilin::VERSION].liLon) {
        tokiENanpaPiIloNi();
        return 0;
    }


    if (lipuWawaPana.empty()) {
        kepeken::tokiEIke({ante_toki::nimiTawaJan("ike.ilo_CLI.ilo_pilin.lipu_lawa_li_wile")});
        std::cout << ante_toki::nimiTawaJan("ike.ilo_CLI.o_lukin_e_sona_kepeken") << '\n';

        return 1;
    }



    std::function<void(const std::string&)> paliWile = &lawaEIloNanpa;

    if (iloPilinAli[NimiPiIloPilin::TOKI_E_IJO].liLon) {
        paliWile = &tokiELipuKipisi;

    } else if (iloPilinAli[NimiPiIloPilin::TOKI_E_KASI].liLon)
        paliWile = &tokiEKasiLipu;

    for (const std::string *const lipuWawa : lipuWawaPana)
        try {
            paliWile(*lipuWawa);

        } catch(const std::runtime_error& liIke) {
            return 1;

        } catch(const std::invalid_argument& lipuLiLonAla) {
            return -1;
        }

    return 0;
}
