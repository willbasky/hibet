module Paths where

testDir :: FilePath
testDir = "test/env/data/"

sylPath, titlePath, dictDir :: FilePath
sylPath = testDir <> "stuff/tibetan-syllables"
titlePath = testDir <>  "stuff/titles.toml"
dictDir = testDir <> "dicts/"

dictPath1 :: FilePath
dictPath1 = dictDir <> "Berzin-T|E.txt"

dictPath2 :: FilePath
dictPath2 = dictDir <> "RichardBarron-T|E.txt"