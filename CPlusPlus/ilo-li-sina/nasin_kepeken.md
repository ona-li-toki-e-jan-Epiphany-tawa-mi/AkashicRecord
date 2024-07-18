# **nasin kepeken pi toki "ilo li sina"**

"ilo li sina" li toki pi lawa e ilo nanpa li lili li tawa taso kama jo en toki e
nimi. taso, kepeken ni taso la sina ken pali e ijo mute.

ali lon toki ni li nimi. sina ken kepeken e nimi taso. sina ken pali e nimi lon
lipu wawa kepeken sitelen pi `"` en `'` en `` ` `` pi poki nimi (sama `"test"`
anu `'ni li nimi'`).

## **ijo pi toki ni**

---------------

### ***nimi wawa***

"ilo li sina" li jo e nimi wawa lili. ona li kama jo e ijo li pali e ijo. tawa
pali la sina sitelen e nimi e poki tawa ijo `"("` en `")"` e ijo lon ona.

ijo tawa nimi wawa li kipisi kepeken sitelen `" "` pi lukin ala. ni li wile ala
tawa ijo ante sama poki en nimi. taso, ni li lukin pona. pana la:

```ilo li sina
    tokiELinja("ni li" " nimi") # pona. li toki e "ni li nimi".
    tokiELinja("ni li"" nimi") # pona.
    a = "ni li"
    tokiELinja(a " nimi") # pona.
    tokiELinja(a" nimi") # pona.
    b = " nimi"
    tokiELinja(a b) # pona.
    tokiELinja(ab) # ike. poki "ab" li jo e ala li ni ala: "a" en "b".
```

sina ken pana e nimi wawa tawa nimi ante:

```ilo li sina
    # li toki e nimi jan.
    tokiELinja("sina toki e ni: " kamaJo())

    tokiELinja("sina jan " kamaJo("nimi sina li seme? "))
```

sina ken pali e nimi wawa sin kepeken `lawa()`. sina wile pana e ali pi nimi
wawa lon poki 1 li wile pana e poki ni tawa `lawa()`. nimi wawa li jo e linja
mute la o pana e sitelen pi linja sin (`\n`) lon pini pi linja ali. tawa lukin
pona la sina ken kepeken e `wan()` anu `wanKepeken()` li ken pana e linja lon
poki nimi ante. pana la:

```ilo li sina
    tokiEToki = "tokiELinja(nimi ` o, toki!`)"

    nimi = __nimi_jan
    lawa(tokiEToki) # li toki e toki tawa jan.
    nimi = "ma"
    lawa(tokiEToki) # li toki e "ma o, toki!".

    tokiPonaEWile = wan("wile = kamaJo(`sina wile e seme? `)                  \n"  \
                        "tokiELinja(wile ` li ijo pona kin.`)                 \n"  \
                        "tokiELinja(`mi wile e ni: sina ken kama jo e ` wile) \n")
    lawa(tokiPonaEWile)
    # $ sina wile e seme? mani
    # $ mani li ijo pona kin
    # $ mi wile e ni: sina ken kama jo e mani
```

sina ante e poki lon `lawa()` la ona li ante lon ali pi lipu wawa sina. kepeken
ni li sina ken kama jo e ijo kama tan nimi wawa lon `lawa()`. o pana e ijo kama
lon poki o kama jo tan poki ni, en, ijo kama li lon ni. pana la:

```ilo li sina
    tokiPonaEWile = wanKepeken("\n"                                 \
            "wile = kamaJo(`sina wile e seme? `)                 "  \
            "tokiELinja(wile ` li ijo pona kin.`)                "  \
            "tokiELinja(`mi wile e ni: sina ken kama jo e ` wile)")
    lawa(tokiPonaEWile)

    tokiELinja("a a a! mi sona e sona suli sina! sina wile e " wile)
```

ni li nimi wawa ale:

#### **toki(\[nimi...\]) -> ala**

li toki e `nimi` tawa jan.

#### **tokiELinja(\[nimi...\]) -> ala**

sama `toki()` la li toki e `nimi`. taso, pini la ona li toki e linja sin.

#### **tokiEIke(\[nimi...\]) -> ala**

sama `toki()` la li toki e `nimi`. taso, li toki e ona lon lupa `Stderr` lon ala
lupa `Stdout`.

#### **tokiEIkeELinja(\[nimi...\]) -> ala**

sama `tokiELinja()` la li toki e `nimi` e linja sin. taso, li toki e ona lon lupa
`Stderr` lon ala lupa `Stdout`.

#### **kamaJo(\[nimi...\]) -> nimi tan jan**

li pana e `nimi` tawa `toki()` li kama jo e `nimi` tan jan. li awen e lipu wawa.
jan li toki e nimi la li open e lipu wawa.

#### **alaEIloPana() -> ala**

*`li lon taso lawa Windows en lawa UNIX.`*

li ala (weka e sitelen) e ilo pana.

#### **wan(nimi nimi \[nimi...\]) -> nimi wan tan nimi pana**

li wan e `nimi` lon `nimi wan`.

#### **wanKepeken(nimiKepeken nimi nimi \[nimi...\]) -> nimi wan tan nimi pana kepeken nimi.**

li wan e `nimi` pana kepeken `nimi kepeken` pana. pana la
`wanKepeken(", " "1" "2" "3")` -> `"1, 2, 3"`.

#### **awen(tenpo \[tenpo...\]) -> ala**

li awen lon tenpo tan wan e `tenpo` pana. `nimi tenpo` li wile sama nanpa pona
pi nanpa lili ala (sama `"400"` anu `"1369"` sama ala `"3.5"` anu `"0.1111"`) li
sama tenpo Milisekon (Miliseconds). `nimi tenpo` li ala la li pali e ala kepeken
ona (pana la sina ken pana e `awen("200" _)`. ni li awen lon tenpo Milisekon
`200` li toki ala e ike).

#### **pilin(nimi nimi \[nimi...\]) -> nimi 1 tan nimi pana**

nasin ala la li pilin e `nimi` 1 tan `nimi` pana.

#### **pokiPiLawaOS(nimiKen \[nimiKen...\]) -> nimi lon poki | ala**

li kama jo e nimi lon poki Enviroment Variables pi lawa OS pi `nimi ken` pi
nanpa 1 pi jo e nimi.

lawa OS (sama Windows anu Linux) li jo e poki Enviroment Variables lon poki
Enviroment. lipu wawa ali li ken lukin e ona. sina ken kama jo e nimi lon poki
ni kepeken nimi wawa ni.

`nimi ken` li nimi ali tawa poki wile. pana la nimi jan li ken lon `USER` anu
`USERNAME` anu `LOGNAME` anu ante. taso, ona li lon ala ona ali. ni la sina ken
pana e ona ali lon `pokiPiLawaOS()` li kama jo e nimi lon poki pi nanpa 1 pi jo
e nimi (taso, tawa ni la sina ken pona kepeken e poki `__nimi_jan`).

poki li lon ala la li pana e `ala`.

#### **lawa(\[linja...\]) -> ala**

li pali e lipu wawa tan `linja` li lawa e ilo nanpa kepeken ona. lawa pi lipu
wawa ni li kepeken e poki sama lipu wawa suli. sina ante e poki lon ona la ona
li ante lon ali pi lipu wawa sina. kepeken ni la sina ken pali e nimi wawa sin.
o lukin lon sewi tawa sona pona.

#### **lawaELipu(lonLipu \[lonLipu...\]) -> ala**

o lukin e [kama jo e poki tan lipu ante.](nasin_kepeken-tok.md#kama-jo-e-poki-tan-lipu-ante "kama jo e poki tan lipu ante")

#### **ikeLaTawaAla() -> ala**

o lukin e ["ike la seme?".](nasin_kepeken-tok.md#ike-la-seme "ike la seme?")

### ***tawa***

toki li kepeken e nimi wawa tawa GOTO tawa tawa lon lipu wawa.

nimi wawa ni li wile e nimi tawa tawa ni: ona li ken tawa lon ona. sina ken pali
e ona kepeken nimi en sitelen `':'` lon monsi. ona li ken lon open linja taso.
pana la:

```ilo li sina
    tawa(nimiTawa)
    tokiELinja("mi toki ala e nimi ni!")
nimiTawa:
```

nimi `tawa()` li tawa lon `nimiTawa:`. tan ni la nimi `tokiELinja()` li toki
ala.

sina ken kepeken e nimi `niLaTawa()` sama ni tawa tawa ken. pana la:

```ilo li sina
awen:
    tokiELinja("mi awen toki e nimi ni!")
    niLaTawa(awen "pini ala" "pini" \
             "pini anu pini ala? ")
    tokiELinja("mi awen ala toki e nimi ni ;(")
```

jan li toki e nimi `"pini ala"` la nimi `niLaTawa()` li tawa lon nimi tawa
`awen:`. en, li awen toki e nimi. taso, jan li toki e `"pini"` la nimi
`niLaTawa()` li tawa ala. en, li awen ala toki li pini.

sin la sina ken kepeken e `alaLaTawa()` tawa tawa ken. nimi ali tawa ona li jo e
ala la li tawa.

```ilo li sina
    tawa(kamaPiJoNimi)
liTokiEAla:
    tokiELinja("sina toki e ala!")
kamaPiJoNimi:
    nimi = kamaJo("nimi sina li seme? ")
    alaLaTawa(liTokiEAla nimi)
    tokiELinja("jan " nimi " o, toki!")
```

jan li toki e ala la `alaLaTawa()` li kama jo e ala li tawa lon nimi
`liTokiEAla:` li kama jo sina e nimi.

`alaLaTawa()` li lon tan ni: nimi wawa lili en ijo ante lili li ike la li pana e
ala (`""`). sina ken kepeken e `alaLaTawa()` tawa kama sona e ni kepeken
`niLaTawa()`. pana la ken la sina wile ala e ni: nimi li ala la jan li sona e
nimi. en, ni li ken pona pona e lipu wawa.

nimi wawa tawa li tawa lon tenpo ni taso: nimi wawa ali en ijo ante ali lon
linja li pini. tan ni la sina ken kepeken e ona sama nimi wawa pi tawa ala. pana
la:

```ilo li sina
    # jan li toki ala e sona pona la li poki e nimi ona lon poki liSonaPona.
ala: liSonaPona = niLaTawa(sonaPona "sona pona" _                                  \
        "kijetesantakalu li nimi pona pona pi toki pona, anu seme? [sona pona] "))

    tokiELinja("sina sona e ala! o toki e \"sona pona\" e \"" liSonaPona "\" ala!")
    awen("1500")
    tawa(ala)

sonaPona:
    tokiELinja("sona pona kin!")
    awen("1500")


    # tokiELinja() li toki e wile jan kepeken nimi tan niLaTawa().
    #
    # sin la niLaTawa() li tawa ala tawa la awen() li awen() li toki e ike ala tan ni:
    #   tokiELinja li pana e ala. awen() li pali e ala kepeken nimi ala.
    awen("1500" tokiELinja(                          \
        "sina pilin "                                \
        niLaTawa(ala "pini ala" "pini"               \
            "sina wile pini ala pini e lipu wawa? ") \
        " e lipu wawa ni"))
```

sina ken pali e tawa ken pi nasin ala kepeken `alaLaTawa()` en `pilin()` sama
ni: o pana e nimi en ala lon kepeken `pilin()`, en, pana e ijo kama tawa
`alaLaTawa()`. `pilin()` li pana e ala la ona li tawa. sina ken ante e nanpa pi
nimi en ala tawa ante e ken tawa. pana la sina wile tawa lon `tenpo 3 tan 10` la
o kepeken e `alaLaTawa(nimiTawa pilin(_ _ _ "1" "2" "3" "4" "5" "6" "7"))`.
nanpa li toki e ala. ijo suli 1 ona li lon: ona li ala ala (sama `'_'`).

ni li nimi wawa tawa ali lon toki "ilo li sina":

#### **tawa(nimiTawa) -> ala**

li tawa lon `nimi tawa`.

#### **niLaTawa(nimiTawa nimiLon nimiPiLonAla \[nimiTawaToki...\]) -> nimi tan jan**

li toki e `nimi` tawa `toki()`. jan li toki e `nimi lon` la li tawa lon
`nimi tawa`. jan li toki e `nimi pi lon ala` la li tawa ala. li tawa anu tawa
ala la li pana e `nimi tan jan`.

sina ken pana e `nimi lon` ala (`""`) anu `nimi pi lon ala` ala. ni la nimi ali
pi nimi ante ala li ken tawa ona. taso, ona tu li ken ala jo e ala lon tenpo sama.

pana la sina pana e `""` tawa `nimi lon` e `"test"` tawa ante la jan li toki e
`"test"` la ona li tawa ala. taso, jan li toki e nimi sama ala `"test"` la ona
li tawa.

`nimi lon` en `nimi pi lon ala` li jo ala e ala la jan li toki ala e ona li toki
e nimi ante la nimi `niLaTawa()` li toki sin e toki seme.

#### **alaLaTawa(nimiTawa nimi \[nimi...\]) -> ala**

`nimi` pana ale li jo e ala la li tawa.

#### **ikeLaTawa(nimiTawa) -> ala**

o lukin e ["ike la seme?".](nasin_kepeken-tok.md#ike-la-seme "ike la seme?")

#### **ike(\[nimi\]) -> ala**

li ike. `nimi` li nimi lon ike li pana tawa jan.

### **poki**

sina ken kepeken e poki tawa poki e nimi. sina ken pali e ona kepeken nimi en
sitelen `'='` en ijo tawa poki. sina ken pana e ona lon nimi wawa anu poki ante
kepeken nimi ona. pana la:

```ilo li sina
    a = "test"
    b = a
    tokiELinja(b) # li toki e "test"

    a = kamaJo("sina wile e seme? ")
    tokiELinja(a " li tawa ala sina") # nimi jan li "mani" la li toki e "mani li tawa ala sina".

    ijo = "ijo"
mute:
    niLaTawa(muteAla "mute ala" "mute"       \
             "li wile mute ala mute e ijo? ")
    ijo = wan(ijo ijo)
    tawa(mute)
muteAla:
    tokiELinja(ijo) # jan li toki e mute lon tenpo 4 la li toki e "ijoijoijoijoijoijoijoijoijoijoijoijoijoijoijoijo".
```

### **ike la seme?**

nimi wawa li ken pali e ike. pana la `awen()` li kama jo e ijo pi nanpa ala la
ona li ike. `lawa()` li kama jo e lipu wawa ike, anu, ike li kama lon ona la ona
li ike. sina pali e ala la ike li kama la lipu wawa li pini. taso, sina ken pali
e ni kepeken `ikeLaTawa()` en `ikeLaTawaAla()`: ike la ona li awen pali.
`ikeLaTawa()` li tawa ala. ona li pali e ni: ike li kama la ilo pi toki "ilo li
sina" li tawa lon nimi tawa ona. pana la:

```ilo li sina
open:
    suli = kamaJo("tenpo sike pi nanpa seme la sina lon? ")

    ikeLaTawa(ike)
    awen(suli)
    tokiELinja("tenpo ni la suli sina li tenpo sike " suli " + " suli " Milisekon!")

    tawa(pini)
ike:
    tokiELinja("'" suli "' li nanpa ala!")
    tawa(open)
pini:
```

jan li toki e nanpa la li awen li pini. taso, ona li toki e ijo pi nanpa ala la
`awen()` li ike la ona li tawa `ike:` li toki e ike tawa ona li tawa open li
toki sin e toki seme.

taso, ni li pini ala. ona li toki e ijo pi nanpa ala la `kamaJo()` li lawa sin
la ona li pilin e nena `Ctrl-D` tawa pini la ona li lukin e ni:

```console
tenpo sike pi nanpa seme la sina lon? nanpa
ilo_li_sina: <nimi lipu>(5,5): ike: awen(): nanpa pi nanpa lili
        ala li wile la li kama jo e ijo 'nanpa' sama ala nanpa
'nanpa' li nanpa ala!
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
tenpo sike pi nanpa seme la sina lon?
...
```

ike a! sina kepeken e `ikeLaTawa()` la nimi wawa pi ken ike li pini la o kepeken
e `ikeLaTawaAla()` tawa weka e ike nasa sama ni. pana la:

```ilo li sina
open:
    suli = kamaJo("tenpo sike pi nanpa seme la sina lon? ")

    ikeLaTawa(ike)
    awen(suli)
    ikeLaTawaAla()

    tokiELinja("tenpo ni la suli sina li tenpo sike " suli " + " suli " Milisekon!")


    tawa(pini)
ike:
    ikeLaTawaAla()
    tokiELinja("'" suli "' li nanpa ala!")
    tawa(open)
pini:
```

### **kama jo e poki tan lipu ante**

ken la sina wile pana e poki e nimi wawa lon lipu ante en kama jo e ona lon lipu
wawa sina. ni li ken kepeken `lawaELipu()`. `lawaELipu()` li lawa e ilo nanpa
kepeken lipu wawa pana li pana e poki e nimi wawa tan ona lon lipu wawa sina.
pana la:

nasinAla.ils:
```ilo li sina
    ##
    # li pana "pilin": sitelen anu nanpa pi nasin ala
    pilin = ""
    pilinESitelenPNA = wanKepeken("\n"                                                         \
            "pilin = pilin('a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' \\"  \
            "              'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z' '1' '2' '3' '4' \\"  \
            "              '5' '6' '7' '8' '9' '0')                                      ")

    ##
    # li pana "pokiNimi": poki nimi pi suli 20 pi nasin ala.
    pokiNimi = ""
    paliEPokiNimiPNA = wanKepeken("\n"                            \
            "pokiNimi = ``                                     "  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)"  \
            "pokiNimi = wan(pokiNimi lawa(pilinESitelenPNA) pilin)")

    ##
    # nimi pona tawa jan suli sina ;).
    nimiPona = "mi olin e sina!"
```

lipuWawaSina.ils:
```ilo li sina
    lawaELipu("nasinAla.ils")

    tokiELinja(lawa(pilinESitelenPNA) pilin)    # ken la li "4".
    tokiELinja(lawa(paliEPokiNimiPNA) pokiNimi) # ken la li "yyea84t3q7vx0x3b9poj7".
    tokiELinja(nimiPona)                        # kin la li "mi olin e sina!".
```

### ***ijo lili***

#### *sitelen nasa:*

sina ken sitelen e sitelen nasa (sama linja sin) kepeken sitelen `'\'`. ni li
ona:

 - `\n` - linja sin.
 - `\t` - sitelen suli pi lukin ala.
 - `\v` - sama `\t`. taso, ona li tawa lon anpa.
 - `\b` - li weka e sitelen 1.
 - `\"` - li ken e ni: sina sitelen e sitelen `"` lon poki nimi.
 - `\'` - li ken e ni: sina sitelen e sitelen `'` lon poki nimi.
 - `` \` `` - li ken e ni: sina sitelen e sitelen `` ` `` lon poki nimi.
 - `\\` - li ken e ni: sina sitelen e sitelen `'\'` lon poki nimi.

pana la:

```ilo li sina
    toki("ni li toki \"ilo li sina\" !\b\b") # li toki e 'ni li toki "ilo li sina"'.
```

#### *ijo lon linja mute:*

"ilo li sina" la linja li kipisi e nimi wawa e pana lon poki e nimi tawa. taso,
sina ken sitelen e ona lon linja mute kepeken sitelen `'\'`. sina pana e ona lon
pini linja la li awen lukin e linja lon anpa. pana la:

```ilo li sina
    abc    \
    =      \
    "sona"
    niLaTawa(liSona abc "sona ala"                     \
            "sina sona ala sona e jan pi pan namako?")

    tokiELinja("toki ni " " li" \
               " lon linja"     \
               " mute a!")

    tawa(pini)

liSona: tokiELinja(       \
                          \
                          \
                          \
                          \
                          \
                          \
                 "pona!")

pini:
```

#### *kama jo pi sitelen EOF (pini lipu):*

nimi wawa `kamaJo()` en `niLaTawa()` li kama jo e sitelen EOF la li pini e lipu
wawa.

ni li tan ni: ali pi toki ni li kama jo e nimi li toki e nimi. nimi li lon ala
tawa kama la "ilo li sina" li ken pali e ala. taso, ona li ken e ni: kama jo e
nimi tan lipu. pana la sina ken pana e nimi wile lon lipu `temp.txt` li sitelen
e `temp.txt | ilo_li_sina <nimi pi lipu wawa>"`. lipu wawa li kama jo e nimi ali
tan `temp.txt` la li pini li awen ala lon tenpo ali.

#### *poki lon open pi lipu wawa ali:*

poki lili li jo e nimi lon open pi lipu wawa ali "ilo li sina".

**__nanpa_Ilo_Li_Sina** - nanpa pali pi ilo tawa lawa kepeken "ilo li sina".

**__nimi_Ilo_Li_Sina** - nasin tawa ilo tawa lawa tan poki lipu pi tenpo ni.

**__nimi_lipu** - nasin tawa lipu wawa tan poki lipu pi tenpo ni.

**__nimi_jan** - li jo e nimi jan tan lawa OS. li ken ala alasa e nimi jan la li
jo e ala.

**_** - nimi lili tawa nimi (`""`) pi jo e ala.
