#include "pali.hpp"

#include <cassert>

#include "../lawa/nimi_wawa.hpp"
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
    /**
     * @brief li pona e pana ijo tawa nimi wawa pali.
     */
    struct SonaPali {
        const std::string& lonLipu;
        KasiOpen&          kasiOpen;

        std::list<Ijo>&           ijoKipisi;
        std::list<Ijo>::iterator& alasaIjo;

        std::unordered_map<std::string, size_t>&                       lonPiNimiTawaTawa;
        std::list<const Ijo*>&                                         nimiTawaTawaPiTenpoNi;
        std::list<std::pair<const std::string&, KasiPiNimiWawaTawa&>>& nimiTawaTawaTawaNimiWawaTawa;

        bool liLipuPona = true;
    };



    /**
     * @brief li tawa e alasa ijo lon linja sin.
     * @attention ike la o kepeken e ni tawa ni: ilo pali li ken awen toki e ike ante lon lipu.
     */
    void tawaLinjaSinLonSinpin(SonaPali& sonaPali) {
            for (; sonaPali.alasaIjo != sonaPali.ijoKipisi.cend(); sonaPali.alasaIjo++)
                if (sonaPali.alasaIjo->nimiIjo == NimiIjo::LINJA_SIN)
                    break;
    }

    /**
     * @return poki nimi sin tan lon pi tenpo ni lon poki pi lipu kipisi.
     * @attention o pali e ni lon monsi pi kepeken ni: nimi pi ijo pi tenpo ni li POKI_NIMI.
     */
    std::unique_ptr<KasiPiPokiNimi> paliEPokiNimi(SonaPali& sonaPali) {
        auto kasiPiPokiNimi = std::make_unique<KasiPiPokiNimi>();

        kasiPiPokiNimi->nimi = std::move(sonaPali.alasaIjo->ijo);
        kasiPiPokiNimi->lonKasi = sonaPali.alasaIjo->lonIjo;
        sonaPali.alasaIjo++;

        return kasiPiPokiNimi;
    }

    /**
     * @return poki sin tan lon pi tenpo ni lon poki pi lipu kipisi.
     * @attention o pali e ni lon monsi pi kepeken ni: nimi pi ijo pi tenpo ni li POKI.
     */
    std::unique_ptr<KasiPoki> paliEPoki(SonaPali& sonaPali) {
        auto kasiPiPoki = std::make_unique<KasiPoki>();

        kasiPiPoki->nimiPoki = std::move(sonaPali.alasaIjo->ijo);
        kasiPiPoki->lonKasi = sonaPali.alasaIjo->lonIjo;
        sonaPali.alasaIjo++;

        return kasiPiPoki;
    }

    /**
     * @return nimi wawa (anu nimi wawa tawa) sin tan lon pi tenpo ni lon poki pi lipu kipisi.
     * @attention o pali e ni lon monsi pi kepeken ni: nimi pi ijo pi tenpo ni li NIMI_WAWA.
     */
    std::unique_ptr<KasiTomoPiNimiWawa> paliENimiWawa(SonaPali& sonaPali) {
        std::unique_ptr<KasiTomoPiNimiWawa> kasiTomoPiNimiWawa = nullptr;
        bool nimiWawaLiLon = false;
        bool liNimiWawaTawa = false;

        try {
            kasiTomoPiNimiWawa = std::make_unique<KasiPiNimiWawa>(
                    &nimiTawaNimiWawa.at(sonaPali.alasaIjo->ijo));
            nimiWawaLiLon  = true;

        } catch (const std::out_of_range& liLonAla) {
            try {
                kasiTomoPiNimiWawa = std::make_unique<KasiPiNimiWawaTawa>(
                        &nimiTawaNimiWawaTawa.at(sonaPali.alasaIjo->ijo));

                nimiWawaLiLon  = true;
                liNimiWawaTawa = true;

            } catch (const std::out_of_range& liLonAla) {}
        }

        if (!nimiWawaLiLon) {
            kepeken::tokiEIke({ sonaPali.lonLipu
                              , sonaPali.alasaIjo->lonIjo
                              , ante_toki::anteENimi( ante_toki::nimiTawaJan("ike.pali.nimi_wawa.nimi_pi_sona_ala")
                                                    , "%s", sonaPali.alasaIjo->ijo)});

            sonaPali.liLipuPona = false;
            return nullptr;
        }

        kasiTomoPiNimiWawa->lonKasi  = sonaPali.alasaIjo->lonIjo;


        // ijo lon sinpin pi nimi wawa li wile open pi poki pi ijo pi nimi wawa. ilo kipisi li pona
        //      la ni li ike lon tenpo ala.
        sonaPali.alasaIjo++;
        assert( sonaPali.alasaIjo != sonaPali.ijoKipisi.end()
             && sonaPali.alasaIjo->nimiIjo == NimiIjo::POKI_PI_IJO_PI_NIMI_WAWA
             && sonaPali.alasaIjo->ijo == "("
             && "ijo lon sinpin pi nimi wawa li wile open pi ijo 'POKI_PI_IJO_PI_NIMI_WAWA'");

        // nimi wawa li tawa la ni li kama jo e nimi ona tawa tawa.
        if (liNimiWawaTawa) {
            sonaPali.alasaIjo++;
            if (sonaPali.alasaIjo == sonaPali.ijoKipisi.end()
                    || sonaPali.alasaIjo->nimiIjo != NimiIjo::POKI) {
                if (sonaPali.alasaIjo == sonaPali.ijoKipisi.end())
                    sonaPali.alasaIjo--;

                kepeken::tokiEIke({ sonaPali.lonLipu
                                  , sonaPali.alasaIjo->lonIjo
                                  , ante_toki::nimiTawaJan("ike.pali.nimi_wawa.tawa.nimi_tawa_tawa_li_wile")});

                sonaPali.liLipuPona = false;
                return nullptr;
            }

            sonaPali.nimiTawaTawaTawaNimiWawaTawa.push_back({
                    sonaPali.alasaIjo->ijo
                  , *static_cast<KasiPiNimiWawaTawa*>(kasiTomoPiNimiWawa.get())});
        }

        for (sonaPali.alasaIjo++; sonaPali.alasaIjo != sonaPali.ijoKipisi.end();)
            switch (sonaPali.alasaIjo->nimiIjo) {
                case NimiIjo::NIMI_WAWA:
                    kasiTomoPiNimiWawa->ijoPiNimiWawa.push_back(std::move(
                            paliENimiWawa(sonaPali)));

                    // li pini pali e linja ni tan ni: ni li ike la ike sona sin li ken ala kama tan linja.
                    if (kasiTomoPiNimiWawa->ijoPiNimiWawa.back() == nullptr)
                        return nullptr;

                    break;

                case NimiIjo::POKI:
                    kasiTomoPiNimiWawa->ijoPiNimiWawa.push_back(std::move(
                            paliEPoki(sonaPali)));
                    break;

                case NimiIjo::POKI_NIMI:
                    kasiTomoPiNimiWawa->ijoPiNimiWawa.push_back(std::move(
                            paliEPokiNimi(sonaPali)));
                    break;

                case NimiIjo::POKI_PI_IJO_PI_NIMI_WAWA:
                    // ilo kipisi li pona la ni li ken ala tan ni: ona li kiwen e ni: sitelen '(' li
                    //      ken lon sinpin pi nimi wawa taso.
                    assert( sonaPali.alasaIjo->ijo == ")"
                         && "ijo 'POKI_PI_IJO_PI_NIMI_WAWA' lon pini poki li wile ')'!");
                    sonaPali.alasaIjo++;

                    goto liPini;

                case NimiIjo::LINJA_SIN:
                    goto liJoAlaEPini;

                default:
                    assert(false && "pali e nimi wawa la li kama jo e ijo pi wile ala!");
            }

        // sin la li alasa e pini lipu la li kama lon ni.
    liJoAlaEPini:
        kepeken::tokiEIke({ sonaPali.lonLipu
                          , sonaPali.alasaIjo->lonIjo
                          , ante_toki::nimiTawaJan("ike.pali.nimi_wawa.li_jo_ala_e_pini")});

        sonaPali.liLipuPona = false;
        return nullptr;

    liPini:
        // li kin e ni: nimi wawa li jo e nanpa ijo sama anu suli tawa nanpa lili lili pi ijo wile pi nimi
        //      wawa.
        if ( kasiTomoPiNimiWawa->tomoPiNimiWawa()->nanpaLiliPiIjoWile != -1
          && kasiTomoPiNimiWawa->ijoPiNimiWawa.size() < kasiTomoPiNimiWawa->tomoPiNimiWawa()->nanpaLiliPiIjoWile) {
            kepeken::tokiEIke({ sonaPali.lonLipu
                              , kasiTomoPiNimiWawa->lonKasi
                              , ante_toki::anteENimi(ante_toki::anteENimi(
                                        ante_toki::nimiTawaJan("ike.pali.nimi_wawa.nanpa_ijo_lili")
                                        , "%s", sonaPali.alasaIjo->ijo)
                                        , "%d", std::to_string(kasiTomoPiNimiWawa->tomoPiNimiWawa()->nanpaLiliPiIjoWile))});

            sonaPali.liLipuPona = false;
            return nullptr;
        }

        // li kin e ni: nimi wawa li jo e nanpa ijo sama anu lili tawa nanpa suli suli pi ijo wile pi nimi
        //      wawa.
        if ( kasiTomoPiNimiWawa->tomoPiNimiWawa()->nanpaSuliPiIjoWile != -1
          && kasiTomoPiNimiWawa->ijoPiNimiWawa.size() > kasiTomoPiNimiWawa->tomoPiNimiWawa()->nanpaSuliPiIjoWile) {
            kepeken::tokiEIke({ sonaPali.lonLipu
                              , kasiTomoPiNimiWawa->lonKasi
                              , ante_toki::anteENimi(ante_toki::anteENimi(
                                        ante_toki::nimiTawaJan("ike.pali.nimi_wawa.nanpa_ijo_suli")
                                        , "%s", sonaPali.alasaIjo->ijo)
                                        , "%d", std::to_string(kasiTomoPiNimiWawa->tomoPiNimiWawa()->nanpaSuliPiIjoWile))});

            sonaPali.liLipuPona = false;
            return nullptr;
        }


        return kasiTomoPiNimiWawa;
    }

    /**
     * @return pana sin lon poki tan lon pi tenpo ni lon poki pi lipu kipisi.
     * @attention o pali e ni lon monsi pi kepeken ni: nimi lon sinpin pi ijo pi tenpo ni li PANA_LON_POKI.
     */
    std::unique_ptr<KasiPiPanaLonPoki> paliEPanaLonPoki(SonaPali& sonaPali) {
        auto kasiPiPanaLonPoki = std::make_unique<KasiPiPanaLonPoki>();

        // ijo wan pi pana lon poki li wile poki.
        if (sonaPali.alasaIjo->nimiIjo != NimiIjo::POKI) {
            kepeken::tokiEIke({ sonaPali.lonLipu
                              , sonaPali.alasaIjo->lonIjo
                              , ante_toki::nimiTawaJan("ike.pali.poki_nanpa.nimi_poki_li_wile")});

            sonaPali.liLipuPona = false;
            return nullptr;
        }

        kasiPiPanaLonPoki->nimiPoki = std::move(sonaPali.alasaIjo->ijo);
        kasiPiPanaLonPoki->lonKasi = sonaPali.alasaIjo->lonIjo;

        // ijo tu sin li wile: sitelen '=' en ijo ante.
        for (int i = 0; i < 2; i++) sonaPali.alasaIjo++;
        if ( sonaPali.alasaIjo == sonaPali.ijoKipisi.end()
          || sonaPali.alasaIjo->nimiIjo == NimiIjo::LINJA_SIN) {
            // li ken ala toki e lon pi ijo ala.
            if (sonaPali.alasaIjo == sonaPali.ijoKipisi.end())
                sonaPali.alasaIjo--;

            kepeken::tokiEIke({ sonaPali.lonLipu
                              , sonaPali.alasaIjo->lonIjo
                              , ante_toki::nimiTawaJan("ike.pali.poki_nanpa.ijo_tawa_poki_li_wile")});

            sonaPali.liLipuPona = false;
            return nullptr;
        }

        // nimi ijo li lon ala nimi "case" la li wile ala tan ni: ona li ken ala lon sinpin pi pana lon poki.
        switch (sonaPali.alasaIjo->nimiIjo) {
            case NimiIjo::NIMI_WAWA:
                kasiPiPanaLonPoki->ijoPana = std::move(paliENimiWawa(sonaPali));

                // li pini pali e linja ni tan ni: ni li ike la ike sona sin li ken ala kama tan linja.
                if (kasiPiPanaLonPoki->ijoPana == nullptr)
                    return nullptr;

                break;

            case NimiIjo::POKI:
                kasiPiPanaLonPoki->ijoPana = std::move(paliEPoki(sonaPali));
                break;

            case NimiIjo::POKI_NIMI:
                kasiPiPanaLonPoki->ijoPana = std::move(paliEPokiNimi(sonaPali));
                break;

            default:
                kepeken::tokiEIke({ sonaPali.lonLipu
                                  , sonaPali.alasaIjo->lonIjo
                                  , ante_toki::nimiTawaJan("ike.pali.poki_nanpa.ijo_ike_tawa_poki")});

                sonaPali.liLipuPona = false;
                return nullptr;
        }

        sonaPali.alasaIjo++;
        return kasiPiPanaLonPoki;
    }

    /**
     * @brief poki nimiTawaTawaPiTenpoNi li kama jo e nimi tawa tawa mute. nimi wawa ona li kama la ni
     *      li kepeken tawa ni: nimi tawa tawa li tawa nimi wawa.
     * @attention ilo pali li pali e linja la o kepeken e ni.
     */
    void wanEKasiENimiTawa(SonaPali& sonaPali) {
        if (sonaPali.nimiTawaTawaPiTenpoNi.empty())
            return;

        for (const auto nimiTawaTawa : sonaPali.nimiTawaTawaPiTenpoNi)
            try {
                sonaPali.lonPiNimiTawaTawa.at(nimiTawaTawa->ijo);

                kepeken::tokiEIke({ sonaPali.lonLipu
                                  , nimiTawaTawa->lonIjo
                                  , ante_toki::anteENimi(ante_toki::anteENimi(ante_toki::anteENimi(
                                        ante_toki::nimiTawaJan("ike.pali.nimi_wawa.tawa.nimi_tawa_mute_mute")
                                      , "%s", nimiTawaTawa->ijo)
                                      , "%d", std::to_string(nimiTawaTawa->lonIjo.nanpaLinja))
                                      , "%d", std::to_string(nimiTawaTawa->lonIjo.nanpaSitelenLonLinja))});
                sonaPali.liLipuPona = false;

            } catch (const std::out_of_range& liLonAla) {
                sonaPali.lonPiNimiTawaTawa[nimiTawaTawa->ijo] = sonaPali.kasiOpen.kasiLonAnpa.size() - 1;
            }

        sonaPali.nimiTawaTawaPiTenpoNi.clear();
    }



    KasiOpen pali(std::list<Ijo>& ijoKipisi, const std::string& lonLipu) {
        KasiOpen openKasi;

        auto alasaIjo = ijoKipisi.begin();
        // li poki e kasi lon nimi tawa tawa tawa wan e ona e nimi wawa tawa lon linja pi nanpa ni.
        std::unordered_map<std::string, size_t> lonPiNimiTawaTawa;
        // li poki e nimi tawa tawa tawa ni: kasi li ken kama jo e nimi tawa tawa ali lon sewi ona.
        std::list<const Ijo*> nimiTawaTawaPiTenpoNi;
        // li poki e nimi wawa tawa en nimi tawa tawa ona tawa wan e nimi wawa tawa e kasi lon nimi
        //      tawa tawa ona. nimi pi poki ni li suli kin, a a a.
        std::list<std::pair<const std::string&, KasiPiNimiWawaTawa&>> nimiTawaTawaTawaNimiWawaTawa;
        SonaPali sonaPali = {lonLipu, openKasi, ijoKipisi, alasaIjo, lonPiNimiTawaTawa, nimiTawaTawaPiTenpoNi, nimiTawaTawaTawaNimiWawaTawa};

        while (alasaIjo != ijoKipisi.end()) {
            while (alasaIjo != ijoKipisi.end() && alasaIjo->nimiIjo == NimiIjo::LINJA_SIN)
                alasaIjo++;
            if (alasaIjo == ijoKipisi.end())
                break;


            if (alasaIjo->nimiIjo == NimiIjo::NIMI_TAWA_TAWA) {
                nimiTawaTawaPiTenpoNi.push_back(&*alasaIjo);
                alasaIjo++;

                if (alasaIjo == ijoKipisi.end())
                    break;
                // nimi tawa tawa li ken taso lon open linja, taso, ona mute li ken lon linja mute. ni
                //  la nimi tawa tawa pi pini ala lon kulupu nimi tawa tawa li wile taso lon linja.
                if (alasaIjo->nimiIjo == NimiIjo::LINJA_SIN)
                    continue;
            }


            if (alasaIjo->nimiIjo == NimiIjo::NIMI_WAWA) {
                openKasi.kasiLonAnpa.push_back(
                        std::move(paliENimiWawa(sonaPali)));

                wanEKasiENimiTawa(sonaPali);

                // ike la li wile tawa linja sin tawa awen toki e ike lon linja ante.
                if (openKasi.kasiLonAnpa.back() == nullptr)
                    tawaLinjaSinLonSinpin(sonaPali);
                continue;
            }

            auto ijoLonSinpin = alasaIjo;
            ijoLonSinpin++;

            if (ijoLonSinpin->nimiIjo == NimiIjo::PANA_LON_POKI) {
                openKasi.kasiLonAnpa.push_back(
                        std::move(paliEPanaLonPoki(sonaPali)));

                wanEKasiENimiTawa(sonaPali);

                // ike la li wile tawa linja sin tawa awen toki e ike lon linja ante.
                if (openKasi.kasiLonAnpa.back() == nullptr)
                    tawaLinjaSinLonSinpin(sonaPali);
                continue;
            }


            // linja li jo ala e nimi wawa anu pana lon poki la li suli ala. mi ken pali e ala li lukin
            //      e linja ante.
            tawaLinjaSinLonSinpin(sonaPali);
        }

        openKasi.kasiLonAnpa.push_back(std::make_unique<KasiPini>());
        openKasi.kasiLonAnpa.back()->lonKasi = {ijoKipisi.back().lonIjo.nanpaLinja + 1, 0};
        // tenpo ni la nimi tawa tawa li lon la ona li lon pini pi lipu wawa li wile e kasi tawa tawa.
        if (!nimiTawaTawaPiTenpoNi.empty())
            wanEKasiENimiTawa(sonaPali);

        // li wan e nimi tawa tawa e nimi wawa tawa.
        for (auto [nimiTawaTawa, nimiWawaTawa] : nimiTawaTawaTawaNimiWawaTawa)
            try {
                nimiWawaTawa.lonTawaTawa = lonPiNimiTawaTawa.at(nimiTawaTawa);

            } catch (const std::out_of_range& liLonAla) {
                kepeken::tokiEIke({ sonaPali.lonLipu
                                  , nimiWawaTawa.lonKasi
                                  , ante_toki::anteENimi(ante_toki::nimiTawaJan(
                                        "ike.pali.nimi_wawa.tawa.nimi_tawa_pi_sona_ala")
                                      , "%s", nimiTawaTawa)});

                sonaPali.liLipuPona = false;
            }

        if (!sonaPali.liLipuPona)
            throw std::runtime_error("lipu wawa ike!");

        return openKasi;
    }
}
