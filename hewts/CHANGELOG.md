# Changelog

`hewts` uses [PVP Versioning][1].

## 0.1.0.0 - 2026-11-07

First release.

### Added

* Conversion between EWTS (Extended Wylie) and Tibetan Unicode in both
  directions: `convertWylieToUnicode` and `convertUnicodeToWylie`.
* Spelling check that reports findings without changing the text
  (`checkSpelling`).
* Sentence splitting and tokenization that preserve every token: syllables,
  punctuation, digits, Tibetan text and foreign text (`splitSentences`,
  `splitSentencesWith`).
* Grammar for the 37 syllable structures with constraint diagnostics: code,
  severity, span, quoted word and message (`Convert.Diagnostic`).
* Settings for input format, checks on/off, `sloppy` aliases, strict mode and
  spacing corrections (`Settings`, `defaultSettings`).
* Public API for parsing, rendering and diagnostics (`Convert`).
* Parity harness against jsewts, Lingua, ewts-js, ewts-rs and the Java EWTS
  converter, a fixed baseline and a journal of accepted divergences.
* Golden, fuzz and property tests for the tokenizer, grammar and renderer.

### Known limitations

* Parity is a ratchet, not 100%: accepted divergences live in
  `test/parity/known_divergences.tsv`.
* Non-canonical sign order is a known gap; edge forms and escapes are listed
  in `test/README.md`.

[1]: https://pvp.haskell.org
