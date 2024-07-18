#include "lawa.hpp"

#include <cassert>
#include <stdexcept>

#include "../../kepeken/ike.hpp"
#include "../../kepeken/lawa_OS.hpp"
#include "../../ante_toki/ante_toki.hpp"

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
    void KasiOpen::lawa(SonaLawa& sonaLawa) const noexcept(false) {
        lawaELipu(*this, sonaLawa.pokiAli, sonaLawa.lonLipu);
        sonaLawa.pokiPali.push("");
    }

    void KasiPini::lawa(SonaLawa& sonaLawa) const {
        sonaLawa.pokiPali.push("");
    }

    void KasiPiPokiNimi::lawa(SonaLawa& sonaLawa) const {
        sonaLawa.pokiPali.push(this->nimi);
    }

    void KasiPoki::lawa(SonaLawa& sonaLawa) const noexcept(false) {
        try {
            sonaLawa.pokiPali.push(sonaLawa.pokiAli.at(this->nimiPoki));

        } catch (const std::out_of_range& liLonAla) {
            paliEIke(sonaLawa, { sonaLawa.lonLipu
                               , this->lonKasi
                               , ante_toki::anteENimi(ante_toki::nimiTawaJan(
                                    "ike.lawa.poki_nimi.li_lukin_kama_jo_tan_poki_pi_sona_ala")
                               , "%s", this->nimiPoki)});
        }
    }

    void KasiPiNimiWawa::lawa(SonaLawa& sonaLawa) const noexcept(false) {
#ifndef NDEBUG
        size_t suliPiPokiPali = sonaLawa.pokiPali.size(); // li lon tawa assert() taso.
#endif

        // li alasa e ijo tan pini tawa open tawa ni: ni li pona e pali pi nimi wawa sin pi toki
        //      "ilo li sina".
        for (auto alasaIjo = this->ijoPiNimiWawa.rbegin()
                ; alasaIjo != this->ijoPiNimiWawa.rend()
                ; alasaIjo++)
            (*alasaIjo)->lawa(sonaLawa);

        assert( sonaLawa.pokiPali.size() - suliPiPokiPali == this->ijoPiNimiWawa.size()
             && "li lawa e ijo pi nimi wawa la nanpa ijo ike lon poki pali!");


#ifndef NDEBUG
        suliPiPokiPali = sonaLawa.pokiPali.size(); // li lon tawa assert() taso.
#endif

        sonaLawa.lonPiKasiPiTenpoNi = &this->lonKasi;
        this->nimiWawa->lawa(sonaLawa, this->ijoPiNimiWawa.size());

        assert( suliPiPokiPali - sonaLawa.pokiPali.size() == this->ijoPiNimiWawa.size() - 1
             && "li lawa e nimi wawa la nanpa ijo ike lon poki pali!");
    }

    void KasiPiNimiWawaTawa::lawa(SonaLawa& sonaLawa) const noexcept(false) {
#ifndef NDEBUG
        size_t suliPiPokiPali = sonaLawa.pokiPali.size(); // li lon tawa assert() taso.
#endif

        // li alasa e ijo tan pini tawa open tawa ni: ni li pona e pali pi nimi wawa sin pi toki
        //      "ilo li sina".
        for (auto alasaIjo = this->ijoPiNimiWawa.rbegin()
                ; alasaIjo != this->ijoPiNimiWawa.rend()
                ; alasaIjo++)
            (*alasaIjo)->lawa(sonaLawa);

        assert( sonaLawa.pokiPali.size() - suliPiPokiPali == this->ijoPiNimiWawa.size()
             && "lawa e ijo pi nimi wawa tawa la nanpa ijo ike lon poki pali!");


#ifndef NDEBUG
        suliPiPokiPali = sonaLawa.pokiPali.size(); // li lon tawa assert() taso.
#endif

        sonaLawa.lonPiKasiPiTenpoNi = &this->lonKasi;
        this->nimiWawaTawa->lawa(sonaLawa, this->ijoPiNimiWawa.size(), this->lonTawaTawa - 1);

        assert( suliPiPokiPali - sonaLawa.pokiPali.size() == this->ijoPiNimiWawa.size() - 1
             && "li lawa e nimi wawa tawa la nanpa ijo ike lon poki pali!");
    }

    void KasiPiPanaLonPoki::lawa(SonaLawa& sonaLawa) const noexcept(false) {
#ifndef NDEBUG
        size_t suliPiPokiPali = sonaLawa.pokiPali.size(); // li lon tawa assert() taso.
#endif

        this->ijoPana->lawa(sonaLawa);

        assert( sonaLawa.pokiPali.size() - suliPiPokiPali == 1
             && "lawa e nimi wawa tawa la nanpa ijo ike lon poki pali!");


        sonaLawa.pokiAli[this->nimiPoki] = std::move(sonaLawa.pokiPali.top());
        sonaLawa.pokiPali.pop();

        sonaLawa.pokiPali.push("");
    }



    void paliEIke(const SonaLawa& sonaLawa, const kepeken::IjoPiTokiIke& ike) noexcept(false) {
        // ikeLaTawa() li kepeken la jan li sona e ike. ni la mi wile ala toki e ni tawa ona.
        if (sonaLawa.ikeLaLonTawaTawa == static_cast<size_t>(-1))
            kepeken::tokiEIke(ike);
        throw std::runtime_error(ike.kamaJoENimiIke());
    }



    void lawaELipu(const KasiOpen& lipuWawa, const std::string& lonLipu) noexcept(false) {
        std::unordered_map<std::string, std::string> pokiAli;
        lawaELipu(lipuWawa, pokiAli, lonLipu);
    }

    void lawaELipu(const KasiOpen& lipuWawa
             , std::unordered_map<std::string, std::string>& pokiAli
             , const std::string& lonLipu) noexcept(false) {
        std::stack<std::string> pokiPali;
        size_t kasiPiTenpoNi    = 0;
        size_t ikeLaLonTawaTawa = static_cast<size_t>(-1);
        SonaLawa sonaLawa = {lonLipu, pokiAli, pokiPali, kasiPiTenpoNi, ikeLaLonTawaTawa, nullptr};

        for (; kasiPiTenpoNi < lipuWawa.kasiLonAnpa.size(); kasiPiTenpoNi++) {
            try {
                lipuWawa.kasiLonAnpa.at(kasiPiTenpoNi)->lawa(sonaLawa);

                // linja li pini la ona li wile pana e ijo 1 taso - ijo ona. ni ala la ike li lon.
                assert( pokiPali.size() == 1
                    && "lawa e nimi wawa la nanpa ijo ike lon poki pali!");
                pokiPali.pop();

            } catch (const std::runtime_error& ikeLiKama) {
                // ike la lon tawa tawa lon tenpo ike li lon la mi tawa lon ona. ni ala la lipu li wile
                //      pini.
                if (ikeLaLonTawaTawa != static_cast<size_t>(-1)) {
                    kasiPiTenpoNi = ikeLaLonTawaTawa;

                    while (!pokiPali.empty())
                        pokiPali.pop();

                } else
                    throw;
            }
        }
    }



    void panaEPokiOpenLonPokiAli( std::unordered_map<std::string, std::string>& pokiAli
                                , const std::string& lonLipu) {
        pokiAli["__nanpa_Ilo_Li_Sina"] = NANPA_PI_ILO_PI_ILO_LI_SINA;
        pokiAli["__nimi_Ilo_Li_Sina"]  = kepeken::kamaJoENimiPiILO_LI_SINA();
        pokiAli["__nimi_lipu"]         = lonLipu;
        pokiAli["__nimi_jan"]          = kepeken::nimiJan();
        pokiAli["_"]                   = "";
    }
}
