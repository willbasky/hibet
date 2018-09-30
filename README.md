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

Input `tibet` in terminal. And then write your `query` with _Wylie_ transcribtion:

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

    etc.

To quit, just input:

    :q

## ToDo

- [x] Lookup throuth all available libaries.
- [ ] Handle unicode (native tibetan script) from input
- [ ] move backand to [lib](https://hackage.haskell.org/package/optparse-applicative-0.14.2.0)

