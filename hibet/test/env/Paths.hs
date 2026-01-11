module Paths where

testDir :: FilePath
testDir = "test/env/data"

sylPath, titlePath, dictDir :: FilePath
sylPath = testDir <> "/" <> "stuff/tibetan-syllables"
titlePath = testDir <> "/" <> "stuff/titles.toml"
dictDir = testDir <> "/" <> "dicts"

dictPath1 :: FilePath
dictPath1 = dictDir <> "/" <> "Hopkins-2015-T|E.txt"

dictPath2 :: FilePath
dictPath2 = dictDir <> "/" <> "RangjungYeshe-T|E.txt"
