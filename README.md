# Tibetan-English translator for CLI

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

![Bod](bod.jpg)

## About

The TibetCli translates with all dictionaries from [tibetan-dictionary](https://github.com/christiansteinert/tibetan-dictionary) project. And it use [Wylie transliteration](https://en.wikipedia.org/wiki/Wylie_transliteration) to input tibetan words yet. There is [convertor](http://www.digitaltibetan.org/cgi-bin/wylie.pl) from native tibetan to wylie translit and back, if you wish.

## Installation

install from [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

    git clone https://github.com/willbasky/TibetCli.git

    cd ./TibetCli

    stack install tibet

## Usage

Input `tibet shell` in terminal. And then write your `query` using _Wylie_ transcribtion:

    $ tibet shell
    What tibetan word to translate?
    > sems
    1. IvesWaldo
    ༔ cognitive act; 1) cognition, awareness, mind, experience, heart; 2) gtso sems [primary mind/ stream of consciousness/ cognitive   act; 3) alaya/ consciousness; 4) attitude, intent; 5) grasping

    2. GatewayToKnowledge
    ༔ cognitive acts

    3. Verbinator
    ༔ Present: {sems} KYT, KYN, TD, LZ, CD, ND, DK, DS, TC. \nPast: {bsams} KYT, KYN, TD, LZ, CD, ND, DK, DS, TC. {sems} CD.\nFuture:   {bsam} KYT, KYN, TD, LZ, CD, ND, DK, DS, TC. \nImperative: {soms} KYT, KYN, TD, LZ, ND, DK, DS, TC. {som} CD.\nVoluntary: KYT, KYN,   TD, CD, ND, DS, TC, (Hoshi 2003).Involuntary:(Tournadre & Dorje 2003: 176).\nTransitive: DS, TC.\nSyntax: [Erg. Abs.] (Tournadre &    Dorje 2003: 176). [Erg. Obl.] (Hackett 2003).\nCharacteristic Example: {yid kyis} TD. {thos don sogs} LZ. {yid la} ND.\nMeaning: 1.    To think. 2. To fancy, imagine CD. To think, to consider DK. To think DS. To examine, think about TC.

    4. Berzin
    ༔ mind
    ༔ sem

    etc...

To quit, just input:

    :q

To get queries of the session, input:

    :h

## ToDo

- [x] Lookup throuth all available libaries.
- [x] move backand to [lib](https://hackage.haskell.org/package/optparse-applicative-0.14.2.0)
- [ ] Handle unicode (native tibetan script) from input

