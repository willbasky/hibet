# Tibetan-English translator for CLI

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## About

The TibetCli translates with all dictionaries from [tibetan-dictionary](https://github.com/christiansteinert/tibetan-dictionary) project. And it use [Wylie transliteration](https://en.wikipedia.org/wiki/Wylie_transliteration) to input tibetan words yet. There is [convertor](http://www.digitaltibetan.org/cgi-bin/wylie.pl) from native tibetan to wylie translit and back, if you wish.

## Installation

install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
    
    git clone https://github.com/willbasky/TibetCli.git
    
    cd ./TibetCli

    stack install tibet

## Usage

Input `tibet` in terminal

    > tibet
    What tibetan word to translate?
    > sems
    1. Hackett-Def2015
    - (PH) mind; aspiration; intention
    - (PH) mind; aspiration; intention

    2. Berzin-Def
    - All levels of awareness (levels of mind) that are tainted with the fleeting stains of the emotional and     cognitive obscurations. All levels of awareness other than rigpa. Translated as "limited awareness."
    - The cognitive activity of merely giving rise to an appearance or mental hologram of something knowable     and cognitivelyengaging with it.

    3. Yoghacharabhumi-glossary
    cittatA. citta. cetas. caitasika; cetana. mAnasa

    4. Hopkins-Comment
    delete Eng trans "[in the sense of have an opinion]"? (SW)

    5. Hopkins-Def2015
    Def.: that which has similarity with mental factors that arise as its [i.e., a main mind's] accompaniers

    6. JimValby
    cognitive act; Mind; citta, (-, body-, finite) mind, minding, thoughts, noetic, responsiveness, mentation,     spirituality,attitudinal cognition, cognitive act, mentation, experiencing, potential for experience,     general forms of experience, attitude, perceptual readiness, noetic-noematic structure, psychology, soul,     spirit, (sometimes abbr. for sems nyid or byang chub sems), "want" particle, primary mind, experiencing     process, Instructions on the cognitive act written by gnyen dpal dbyangs, thought process, ordinary mind,     attention, attentive mind, conscious mind, state of mind

    7. DanMartin
    cetas, citta, mind, thought. Sometimes used as an abbr. for Bodhicitta. Frequently a word for 'mind' that     is not 'clearly comprehended' (byang chub), in which case it is may be regarded as the special power of     Total Knowledge, but not Total Knowledge itself. For the distinction between sems and rig pa see 91 I     579.3 ff. There are said to be 24 distinctions (I 581.2). It obscures the Total Knowledge substantiality     (I 581.5).

    8. Hopkins-Skt2015
    - {LCh,MV,MSA,C}citta; {C}cittatā; {C,MSA,MV}cetas; {MSA}cetanā; {MSA}cetasika; {MSA}caitasika; {C,MSA}    manas; {C}manyate(=samanupaśyati); {C}matī; {MSA}(anv √īkṣ): anvīkṣati; {MSA}(... snyam sems so = (√vṛt):     iti vartate); cittam; saṃtāna
    - citta

    9. Hopkins-TibetanDefinitions2015
    mtshan nyid/  rang gi 'khor du byung ba'i sems byung dang mtshungs par ldan pa/

    10. RichardBarron
    (ordinary) mind

    11. Hopkins2015
    - verb: think [in the sense of have an opinion]; contemplate; noun: mind; consciousness; thought;     continuum
    - mind

    12. Berzin
    - mind
    - sem

    13. IvesWaldo
    cognitive act; 1) cognition, awareness, mind, experience, heart; 2) gtso sems [primary mind/ stream of     consciousness/ cognitive act; 3) alaya/ consciousness; 4) attitude, intent; 5) grasping

## ToDo

- [x] Lookup throuth all available libaries.
- [ ] Handle unicode (native tibetan script) from input
- [ ] move backand to [lib](https://hackage.haskell.org/package/optparse-applicative-0.14.2.0)

