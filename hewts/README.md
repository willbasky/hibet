# hewts

[![Hackage](https://img.shields.io/hackage/v/hewts.svg?logo=haskell)](https://hackage.haskell.org/package/hewts)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

`hewts` is the Tibetan spelling and conversion layer of
[hibet](https://github.com/willbasky/hibet). It reads Tibetan script and EWTS
(Extended Wylie), converts in both directions, and reports spelling findings
without rewriting the input.

It is a library, not a translator: `hibet` translates, `hewts` says what the
syllables are and whether they are spelled legally. The grammar follows the 37
syllable structures of Nyima Tashi's formal model.

## What it does

- **Wylie to Unicode** (`convertWylieToUnicode`) and **Unicode to Wylie**
  (`convertUnicodeToWylie`, EWTS).
- **Spelling check** (`checkSpelling`) that returns findings and no text.
- **Token stream** (`splitSentences`, `splitSentencesWith`) that keeps every
  token: syllables, punctuation, digits, Tibetan text and bracketed foreign
  text.
- **Settings** (`Settings`, `defaultSettings`): input format, checks off,
  `sloppy` aliases, strict findings.
- **Diagnostics** (`Convert.Diagnostic`): code, severity, span, quoted word and
  message.

## Install

Add the library to a project:

```
build-depends: hewts
```

Or build it from this repository:

```
cd hewts
cabal build hewts
```

## Usage

Every entry point returns `Either Text a`: `Left` carries the parse error,
`Right` the text or the findings. The transcript below is checked by
`Test.Readme`, so these are the library's current answers. Start a repl with:

```
cd hewts
cabal repl lib:hewts
```

and set the session up once:

```ghci
ghci> :set -XOverloadedStrings
ghci> import Convert
ghci> import Convert.Diagnostic
ghci> import qualified Data.Text.IO as TIO
ghci> let wylie = defaultSettings{inputFormat = InWylie}
```

### Wylie to Tibetan script

```ghci
ghci> either TIO.putStrLn (TIO.putStrLn . fst) (convertWylieToUnicode defaultSettings "bkra shis")
བཀྲ་ཤིས
```

### Tibetan script to Wylie

```ghci
ghci> either TIO.putStrLn (TIO.putStrLn . fst) (convertUnicodeToWylie defaultSettings "བཀྲ་ཤིས")
bkra shis
```

### Spelling check

`checkSpelling` does not touch the text; it returns the findings. The codes are
the `DiagnosticCode` constructors:

```ghci
ghci> print (fmap (map diagCode . diagnosticList) (checkSpelling wylie "bk"))
Right [MissingVowelAfterPrefix]
```

`b` here is a prefix with no vowel after it. A clean syllable returns
`Right []`.

### Diagnostics

Each finding carries its position and message. `renderDiagnostics` turns them
into the channel format `line N: "word": message`, with the word quoted when
the rule blames one:

```ghci
ghci> either TIO.putStrLn (mapM_ TIO.putStrLn) (fmap (renderDiagnostics "bk") (checkSpelling wylie "bk"))
line 1: "bk": The stack the prefix "b" leads carries no vowel.
ghci> either TIO.putStrLn (mapM_ TIO.putStrLn) (fmap (renderDiagnostics "tgra") (checkSpelling wylie "tgra"))
line 1: "tgra": The letter "t" cannot be a prefix.
```

### Settings

```ghci
ghci> print defaultSettings
Settings {inputFormat = InUnicode, withChecks = True, sloppy = False, strict = False}
```

`withChecks = False` keeps the conversion but records nothing:

```ghci
ghci> either TIO.putStrLn (print . map diagCode . diagnosticList . snd) (convertWylieToUnicode wylie "tgra")
[InvalidPrefix]
ghci> either TIO.putStrLn (print . map diagCode . diagnosticList . snd) (convertWylieToUnicode wylie{withChecks = False} "tgra")
[]
```

`sloppy = True` applies the reference's input corrections before reading: case
folding, `S` to `s`, and a lone consonant taking its implicit vowel:

```ghci
ghci> either TIO.putStrLn (TIO.putStrLn . fst) (convertWylieToUnicode wylie{sloppy = True} " b m ")
བ་མ་
```

`strict = True` adds the findings that are rare rather than wrong
(`AmbiguousSpelling`, `IllegalSuperfixCombination`); real defects are reported
in both modes:

```ghci
ghci> print (fmap (map diagCode . diagnosticList) (checkSpelling wylie "rkwa"))
Right []
ghci> print (fmap (map diagCode . diagnosticList) (checkSpelling wylie{strict = True} "rkwa"))
Right [IllegalSuperfixCombination]
```

### Syllables

`syllables` drops everything that is not a syllable and returns the recognized
ones in canonical spelling:

```ghci
ghci> either TIO.putStrLn (mapM_ TIO.putStrLn) (syllables "བཀྲ་ཤིས")
བཀྲ
ཤིས
```

## Build and tests

Run from the `hewts/` directory: the suite reads `src/`, `README.md` and
`test/vectors/` by relative path.

```
cabal build hewts
cabal test hewts-test
cabal run hewts-benchmark
```

The benchmark reads `test/vectors/kang.txt` and fails if doubling the input
slows the parse by more than 2.5 times.

## Documentation

- [`../docs/hewts/SPEC.md`](../docs/hewts/SPEC.md) - what the library must do.
- [`../docs/hewts/DESIGN.md`](../docs/hewts/DESIGN.md) - how it is built and why.
- [`../docs/hewts/ROADMAP.md`](../docs/hewts/ROADMAP.md) - open work.
- [`test/README.md`](test/README.md) - test layers, reference vectors and the
  parity ratchet.

## License

BSD-3-Clause, see [LICENSE](LICENSE).
