#!/usr/bin/env python3

import pathlib
import os
import sys

# MIT License
#
# Copyright (c) 2022 ona-li-toki-e-jan-Epiphany-tawa-mi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

# lipu ni li tawa pali e poki Map tan nimi nimi (sama 'test.example' tawa nimi tawa jan
#   sama 'This is an example' anu 'Это пример'.
#
# sina wile pali e ala. ilo Cmake li lawa e ilo nanpa kepeken lipu ni tawa pali e poki.
#
# poki li kama lon 'src/ante_toki/nimi_toki.hpp'

# sina ken ala kepeken e sitelen '\' lon poki Fstring. tan ni la sitelen nasa li poki lon poki nanpa
#   li pana lon ona lon tenpo kama.
sitelenTAB = '\t'
pokiPiLipuToki = r'nimi_en_toki/'
pokiTawaNimiWawa = r'src/ante_toki/nimi_toki.hxx'

# ilo Cmake li kepeken e poki pi lipu ni ala. taso, mi wile e poki pi lipu ni tawa kama jo e lipu pi nimi toki.
# ni li pona e ni.
os.chdir(os.path.dirname(sys.argv[0]))
# nimi pi lipu pi nimi toki li kepeken tawa nimi pi poki Map en nimi toki lon poki pi poki Map ali
lipuTokiEnNimi = [(lipu, lipu.name.split('.', maxsplit=1)[0]) for lipu in pathlib.Path(pokiPiLipuToki).iterdir() if lipu.is_file()]
with open(pokiTawaNimiWawa, mode='w', encoding='utf-8') as nimiWawaSin:
    nimiWawaSin.writelines([
        '#pragma once\n',
        '\n',
        '#include <string>\n',
        '#include <unordered_map>\n'])
    nimiWawaSin.writelines([
        '\n',
        '/**\n',
        ' * @attention sina o, lipu wawa ni li pali kepeken ala jan. o ante ala e ona.\n',
        ' *\n',
        ' * lipu ni li jo e ken pi ante e nimi nimi lon nimi toki.\n'
        ' * sona mute la sina pana e ijo sama nimi "tawa_jan.toki" la sina kama jo e "toki" lon toki pona\n'
        ' * \t\te "Hello" lon toki Inli e "Привет" lon toki Losi.\n'
        ' *\n'
        ' * sina wile e ni: lipu ni li kama jo e nimi sin la o lawa e ilo nanpa kepeken\n',
        ' * lipu "paliELipuToki.py".\n',
        ' */\n'])


    nimiWawaSin.write("namespace ante_toki {")

    for lipu, nimi in lipuTokiEnNimi:
        nimiWawaSin.writelines([
            '\n',
            f'{sitelenTAB}const std::unordered_map<std::string, std::string> {nimi} = {{\n'])

        with lipu.open(encoding='utf-8') as nimiToki:
            linjaLonSinpin = nimiToki.readline()
            nanpaLinja = 0

            # nimi wawa mute lon ni li wile taso tawa toki e sitelen ',' lon lon wile.
            while linjaLonSinpin != '':
                nanpaLinja = nanpaLinja + 1

                # li ken e nimi sona pi lawa ala kepeken sitelen '#'.
                linjaLonSinpin = linjaLonSinpin.split('#', maxsplit=1)[0].strip('\n')
                if linjaLonSinpin == '':
                    linjaLonSinpin = nimiToki.readline()
                    continue

                try:
                    # li tu e nimi nimi e nimi tawa jan kepeken sitelen '='.
                    # li weka e sitelen pi lukin ala tan nimi nimi en nimi tawa jan.
                    nimiNimi, nimiTawaJan = [nimi.strip() for nimi in linjaLonSinpin.split('=', maxsplit=1)]
                    nimiWawaSin.write(f'{sitelenTAB}{sitelenTAB}{{"{nimiNimi}", "{nimiTawaJan}"}}')

                except ValueError:
                    sys.exit(f'{sys.argv[0]}: {lipu}: linja pi nanpa {nanpaLinja}: sitelen "=" pi tu e ijo li wile lon ni')

                linjaLonSinpin = nimiToki.readline()
                if linjaLonSinpin != '':
                    nimiWawaSin.write(',')

                nimiWawaSin.write('\n')

            nimiWawaSin.write('\t};\n')


    # li pali e poki pi poki Map ali tawa pilin e poki tawa toki jan.
    nimiWawaSin.writelines([
        '\n',
        '\tconst std::unordered_map<std::string, const std::unordered_map<std::string, std::string>&> nimiTokiAli = {\n'])

    for nanpa in range(0, len(lipuTokiEnNimi)):
        _, nimi = lipuTokiEnNimi[nanpa]
        nimiWawaSin.write(f'{sitelenTAB}{sitelenTAB}{{"{nimi}", {nimi}}}')

        if nanpa + 1 != len(lipuTokiEnNimi):
            nimiWawaSin.write(',')

        nimiWawaSin.write('\n')

    nimiWawaSin.writelines([
        '\t};\n',
        '}'])
