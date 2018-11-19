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

Help commands:

    $ tibet -h
    
    Usage: tibet [-v|--version] COMMAND
      Translate from Tibetan to English

    Available options:
      -h,--help                Show this help text
      -v,--version             Show TibetCli's version

    Available commands:
      shell                    Start translate shell
      om                       Print Om to a terminal
      show                     Show commands

    $ tibet show -h
    
    Usage: tibet show COMMAND
      Show commands

    Available options:
      -h,--help                Show this help text

    Available commands:
      names                    Show dictionary titles
      meta                     Show dictionary descriptions

Input `tibet shell` in terminal. And then write your `query` using _Wylie_ transcribtion:

    $ tibet shell
    Which a tibetan word to translate?
    > mo
    1. Rangjung Yeshe
    ► the girl
    ► lady, female, woman. 2) divination, prophecy
    ► 1) auspicious, mo divination/ prophesy [R]; 2) feminine particle, female, woman, she [R]; 3) terminative after MA; 4) noun    forming particle [R]; 5) is [don ci mo: what is the meaning]

    2. Ives Waldo
    ► 1) auspicious, mo divination [mo rgyag mkhan gyis mo ma btab gong du bden pa bdar khul gyis mi la mgo skor gtong ba; 2) femine showing particle [bya mo]; 3) terminative after MA; 4) noun forming particle [ring mo, chen mo,//ltad mo,//nub mo]. 1) auspicious, mo divination/ prophesy [R]; 2) feminine particle, female, woman, she [R]; 3) terminative after MA; 4) noun forming particle [R]; 5) is [don ci mo: what is the meaning]. 1) auspicious, mo divination/ prophesy; 2) feminine particle, female, woman, she; 3) terminative after MA; 4) noun forming particle; 5) is

    3. Hopkins 2015
    ► (1) female; woman; wife; feminine particle; she; (2) particle indicating the end of a statement

    4. Berzin
    ► prognostication

    etc...

To quit, just input:

    :q

To get queries of the session, input:

    :h

## ToDo

- [x] Lookup throuth all available libaries.
- [x] move backand to [lib](https://hackage.haskell.org/package/optparse-applicative-0.14.2.0)
- [ ] Handle unicode (native tibetan script) from input

