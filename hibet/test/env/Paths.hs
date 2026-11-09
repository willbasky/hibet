module Paths where

testDir :: FilePath
testDir = "test/env/data"

titlePath, aliasPath, dictDir :: FilePath
titlePath = testDir <> "/" <> "titles.toml"
aliasPath = testDir <> "/" <> "key-aliases.tsv"
dictDir = testDir <> "/" <> "dicts"

dictPath1 :: FilePath
dictPath1 = dictDir <> "/" <> "Hopkins-2015-T|E.txt"

dictPath2 :: FilePath
dictPath2 = dictDir <> "/" <> "RangjungYeshe-T|E.txt"
