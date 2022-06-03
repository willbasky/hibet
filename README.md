# Tibetan-English translator for CLI

[![Haskell-CI](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/willbasky/hibet/actions/workflows/haskell-ci.yml)
[![BSD3 license](https://img.shields.io/badge/license-bsd3-brightgreen)](LICENSE)

![Bod](bod.png)

## About

The Hibet translates with all dictionaries from [tibetan-dictionary](https://github.com/christiansteinert/tibetan-dictionary) project. It consume both [Wylie transliteration](https://en.wikipedia.org/wiki/Wylie_transliteration) and [tibetan script](https://en.wikipedia.org/wiki/Tibetan_script) from input.

## Features

1. Hibet uses about 40 dictionaries to translate Tibetan words to English or Tibetan.
2. If translations have Tibetan texts it printed as wylie.

## Installation

1. Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

2. Install hibet

        git clone https://github.com/willbasky/hibet.git

        cd ./hibet

        stack install hibet

## Upgrade

        stack purge

        stack install hibet

## Usage video

[![asciicast](https://asciinema.org/a/271020.svg)](https://asciinema.org/a/271020)

## Help commands:

`$ hibet -h`

    Hibet is command line translator from Tibet to English language.

    Usage: hibet [-v|--version] [[-s|--select DICT_ID] | COMMAND]

    Available options:
      -h,--help                Show this help text
      -v,--version             Show Hibet's version
      -s,--select DICT_ID      Select id of dictionary

    Available commands:
      shell                    Start the translate shell
      om                       Print Om to a terminal
      show                     Show names or meta of dictionaries
      debug                    Debug hibet


`$ hibet show -h`

    Usage: hibet show COMMAND
      Show names or meta of dictionaries

    Available options:
      -h,--help                Show this help text

    Available commands:
      names                    Show dictionary titles
      meta                     Show dictionary descriptions

`$ hibet shell -h`

    Usage: hibet shell [-s|--select DICT_ID]
      Start the translate shell

    Available options:
      -h,--help                Show this help text
      -s,--select DICT_ID      Select id of dictionary


Input `hibet shell` in terminal. And then write your `query` using _Wylie_ (mo) or _Tibet script_ (མོ་):

    $ hibet shell -s{7,16}
    Which a tibetan word to translate?
    > mo

    མོ

    7. Rangjung Yeshe
      the girl
      lady, female, woman. 2) divination, prophecy
      1) auspicious, mo divination/ prophesy [R]; 2) feminine particle, female,
      woman, she [R]; 3) terminative after MA; 4) noun forming particle [R]; 5)
      is [don ci mo: what is the meaning]

    16. Ives Waldo
      1) auspicious, mo divination [mo rgyag mkhan gyis mo ma btab gong du bden
      pa bdar khul gyis mi la mgo skor gtong ba; 2) femine showing particle
      [bya mo]; 3) terminative after MA; 4) noun forming particle [ring mo,
      chen mo,//ltad mo,//nub mo]. 1) auspicious, mo divination/ prophesy [R];
      2) feminine particle, female, woman, she [R]; 3) terminative after MA; 4)
      noun forming particle [R]; 5) is [don ci mo: what is the meaning]. 1)
      auspicious, mo divination/ prophesy; 2) feminine particle, female, woman,
      she; 3) terminative after MA; 4) noun forming particle; 5) is



### To quit, just input:

    :q

### To get queries of the session, input:

    :h

or use arrows of keyboard.

## Contribution

Contribution is welcome!
