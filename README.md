# Tibetan-English translator for CLI

[![Hackage](https://img.shields.io/hackage/v/tibet.svg)](https://hackage.haskell.org/package/tibet)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/tibet/badge/lts)](http://stackage.org/lts/package/tibet)
[![Stackage Nightly](http://stackage.org/package/tibet/badge/nightly)](http://stackage.org/nightly/package/tibet)

## About

The TibetCli maked with dictionaries from [tibetan-dictionary](https://github.com/christiansteinert/tibetan-dictionary) project. `TibetCli` use Berzin (03-berzin) dictionary yet. And it use [Wylie transliteration](https://en.wikipedia.org/wiki/Wylie_transliteration) to input tibetan words. There is [convertor](http://www.digitaltibetan.org/cgi-bin/wylie.pl) from native tibetan to wylie translit and back, if you wish.

## Installation

    git clone https://github.com/willbasky/TibetCli.git

    stack install tibet

## Usage

Input `tibet` in terminal

    > tibet
    What tibetan word to translate?
    Put query: du shes
    recognition

## ToDo

- [ ] Lookup throuth all available libaries.
- [ ] Handle unicode (native tibetan script) from input
- [ ] move backand to [lib](https://hackage.haskell.org/package/optparse-applicative-0.14.2.0)