module Paths where

home :: FilePath
home = "/home/metaxis/sources/Haskell/apps/hibet/"

testDir :: FilePath
testDir = "test/env/data/"

sylPath, titlePath, dictDir, dictDirAbs :: FilePath
sylPath = testDir <> "stuff/tibetan-syllables"
titlePath = testDir <>  "stuff/titles.toml"
dictDir = testDir <> "dicts/"
dictDirAbs = home <> dictDir

dictPath1 :: FilePath
dictPath1 = dictDirAbs <> "Berzin-T|E.txt"

dictPath2 :: FilePath
dictPath2 = dictDirAbs <> "RichardBarron-T|E.txt"
