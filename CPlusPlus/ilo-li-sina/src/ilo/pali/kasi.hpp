#pragma once

#include <vector>

#include "../lawa/nimi_wawa.hpp"
#include "../../kepeken/ike.hpp"
#include "../lawa/sona_lawa.hpp"

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
 * @brief kasi ali tawa pali e kasi AST pi lipu wawa kepeken ilo pali.
 */
namespace ilo {
    /**
     * @brief li poki e lipu wawa lon kasi tawa lawa e ilo nanpa kepeken tenpo lili.
     */
    class KasiLipu {
        public:
            /**
             * @brief lon kasi lon lipu wawa.
             */
            kepeken::LonIjo lonKasi = {static_cast<size_t>(-1), static_cast<size_t>(-1)};

            /**
             * @return KasiLipu* kasi sin sama kasi ni.
             * @attention o kepeken e ni e ilo Copy Constructor ala (taso, sina ken en wile kepeken e
             *      ona tawa pali e nimi wawa ni.)
             */
            virtual KasiLipu* paliSama() const = 0;
            virtual ~KasiLipu() = 0;

            /**
             * @return nimi kasi tawa toki tawa jan kepeken ante_toki::nimiTawaJan().
             */
            virtual std::string nimiPiNimiKasi() const = 0;

            /**
             * @brief ilo lawa li lawa kepeken nimi ni tawa pali e ijo. nimi wawa kiwen li lon
             *      <../lawa/lawa.cpp>
             * @attention li wile pana e nimi lon poki pali. li jo e ala tawa ni la o pana e "".
             */
            virtual void lawa(SonaLawa& sonaLawa) const = 0;
    };

    /**
     * @brief li kasi pi nanpa 1 lon kasi pi lipu wawa.
     */
    class KasiOpen : public KasiLipu {
        public:
            /**
             * @brief kasi lon ni li open pi linja ali lon lipu wawa.
             */
            std::vector<std::unique_ptr<KasiLipu>> kasiLonAnpa;

            KasiOpen();
            KasiOpen& operator=(const KasiOpen&) = delete;
            virtual KasiOpen* paliSama() const override;
            KasiOpen(KasiOpen&& ante) noexcept;
            KasiOpen& operator=(KasiOpen&& ante) noexcept;
            virtual ~KasiOpen() override;

            virtual std::string nimiPiNimiKasi() const override;

            virtual void lawa(SonaLawa& sonaLawa) const override;

        private:
            KasiOpen(const KasiOpen& ante);
    };

    /**
     * @brief tenpo ali la li lon pini
     */
    class KasiPini : public KasiLipu {
        public:
            KasiPini() = default;
            KasiPini& operator=(const KasiPini&)          = delete;
            virtual KasiPini* paliSama() const override;
            KasiPini(KasiPini&& ante) noexcept            = default;
            KasiPini& operator=(KasiPini&& ante) noexcept = default;
            virtual ~KasiPini() override;

            virtual std::string nimiPiNimiKasi() const override;

            virtual void lawa(SonaLawa& sonaLawa) const override;

        private:
            KasiPini(const KasiPini& ante) = default;
    };

    /**
     * @brief li kama jo e nimi tan poki nimi.
     */
    class KasiPiPokiNimi : public KasiLipu {
        public:
            /**
             * @brief nimi lon poki nimi.
             */
            std::string nimi;

            KasiPiPokiNimi() = default;
            KasiPiPokiNimi& operator=(const KasiPiPokiNimi&) = delete;
            virtual KasiPiPokiNimi* paliSama() const override;
            KasiPiPokiNimi(KasiPiPokiNimi&& ante) noexcept;
            KasiPiPokiNimi& operator=(KasiPiPokiNimi&& ante) noexcept;
            virtual ~KasiPiPokiNimi() override;

            virtual std::string nimiPiNimiKasi() const override;

            virtual void lawa(SonaLawa& sonaLawa) const override;

        private:
            KasiPiPokiNimi(const KasiPiPokiNimi& ante) = default;
    };

    /**
     * @brief li kama jo e nimi tan poki.
     */
    class KasiPoki : public KasiLipu {
        public:
            /**
             * @brief nimi pi poki ni.
             */
            std::string nimiPoki;

            KasiPoki() = default;
            KasiPoki& operator=(const KasiPoki&) = delete;
            virtual KasiPoki* paliSama() const override;
            KasiPoki(KasiPoki&& ante) noexcept;
            KasiPoki& operator=(KasiPoki&& ante) noexcept;
            virtual ~KasiPoki() override;

            virtual std::string nimiPiNimiKasi() const override;

            /**
             * @throws std::runtime_error poki li lon ala.
             */
            virtual void lawa(SonaLawa& sonaLawa) const noexcept(false) override;

        private:
            KasiPoki(const KasiPoki& kasiPoki) = default;
    };

    /**
     * @brief li tomo pi kasi pi nimi wawa en nimi wawa tawa.
     */
    class KasiTomoPiNimiWawa : public KasiLipu {
        public:
            /**
             * @brief ni li ijo tawa nimi wawa. tenpo 1 la ona li lawa. tenpo 2 la ijo tan lawa ona li
             *      pana tawa nimi wawa.
             */
            std::vector<std::unique_ptr<KasiLipu>> ijoPiNimiWawa;

            KasiTomoPiNimiWawa() = default;
            KasiTomoPiNimiWawa& operator=(const KasiTomoPiNimiWawa&) = delete;
            KasiTomoPiNimiWawa(KasiTomoPiNimiWawa&& ante) noexcept;
            KasiTomoPiNimiWawa& operator=(KasiTomoPiNimiWawa&& ante) noexcept;

            virtual const TomoPiNimiWawa* tomoPiNimiWawa() const = 0;

        private:
            KasiTomoPiNimiWawa(const KasiTomoPiNimiWawa& ante);
    };

    /**
     * @brief li kepeken pi nimi wawa.
     */
    class KasiPiNimiWawa : public KasiTomoPiNimiWawa {
        public:
            const NimiWawa* nimiWawa = nullptr;

            KasiPiNimiWawa() = default;
            KasiPiNimiWawa(const NimiWawa* nimiWawa);
            KasiPiNimiWawa& operator=(const KasiPiNimiWawa&) = delete;
            virtual KasiPiNimiWawa* paliSama() const override;
            KasiPiNimiWawa(KasiPiNimiWawa&& ante) noexcept;
            KasiPiNimiWawa& operator=(KasiPiNimiWawa&& ante) noexcept;
            virtual ~KasiPiNimiWawa() override;

            virtual std::string nimiPiNimiKasi() const override;
            virtual const TomoPiNimiWawa* tomoPiNimiWawa() const override;

            /**
             * @throws std::runtime_error ike li kama tan nimi wawa anu lawa e ijo pi nimi wawa.
             */
            virtual void lawa(SonaLawa& sonaLawa) const noexcept(false) override;

        private:
            KasiPiNimiWawa(const KasiPiNimiWawa& ante);
    };

    /**
     * @brief li kepeken pi nimi wawa tawa.
     */
    class KasiPiNimiWawaTawa : public KasiTomoPiNimiWawa {
        public:
            const NimiWawaTawa* nimiWawaTawa = nullptr;
            /**
             * @brief nimi wawa tawa li wile tawa la li tawa lon kasi pi nanpa ni lon kasi open.
             */
            size_t lonTawaTawa               = static_cast<size_t>(-1);

            KasiPiNimiWawaTawa() = default;
            KasiPiNimiWawaTawa(const NimiWawaTawa* nimiWawaTawa);
            KasiPiNimiWawaTawa& operator=(const KasiPiNimiWawaTawa&) = delete;
            virtual KasiPiNimiWawaTawa* paliSama() const override;
            KasiPiNimiWawaTawa(KasiPiNimiWawaTawa&& ante) noexcept;
            KasiPiNimiWawaTawa& operator=(KasiPiNimiWawaTawa&& ante) noexcept;
            virtual ~KasiPiNimiWawaTawa() override;

            virtual std::string nimiPiNimiKasi() const override;
            virtual const TomoPiNimiWawa* tomoPiNimiWawa() const noexcept(false) override;

            /**
             * @throws std::runtime_error ike li kama tan nimi wawa anu lawa e ijo pi nimi wawa.
             */
            virtual void lawa(SonaLawa& sonaLawa) const noexcept(false) override;

        private:
            KasiPiNimiWawaTawa(const KasiPiNimiWawaTawa& ante);
    };

    /**
     * @brief li pana e ijo lon poki pi nimi pana.
     */
    class KasiPiPanaLonPoki : public KasiLipu {
        public:
            /**
             * @brief nimi poki tawa poki e ijo.
             */
            std::string nimiPoki;
            /**
             * @brief tenpo 1 la ni li lawa. tenpo 2 la li pana e ijo kama lon poki.
             */
            std::unique_ptr<KasiLipu> ijoPana;

            KasiPiPanaLonPoki() = default;
            KasiPiPanaLonPoki& operator=(const KasiPiPanaLonPoki&) = delete;
            virtual KasiPiPanaLonPoki* paliSama() const override;
            KasiPiPanaLonPoki(KasiPiPanaLonPoki&& ante) noexcept;
            KasiPiPanaLonPoki& operator=(KasiPiPanaLonPoki&& ante) noexcept;
            virtual ~KasiPiPanaLonPoki() override;

            virtual std::string nimiPiNimiKasi() const override;

            /**
             * @throws std::runtime_error ike li kama tan lawa e ijo pana.
             */
            virtual void lawa(SonaLawa& sonaLawa) const noexcept(false) override;

        private:
            KasiPiPanaLonPoki(const KasiPiPanaLonPoki& ante);
    };
}
