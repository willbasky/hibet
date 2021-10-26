module Paths where

home :: FilePath
home = "/home/metaxis/sources/Haskell/apps/hibet/"

testDir :: FilePath
testDir = "test/env/data/"

dictPath1 :: FilePath
-- dictPath1 = home <> testDir <> "Berzin-T|E.txt"
dictPath1 = "/home/metaxis/sources/Haskell/apps/hibet/" <> "test/env/data/" <> "Berzin-T|E.txt"

dictPath2 :: FilePath
dictPath2 = home <> testDir <> "RichardBarron-T|E.txt"

sylPath, titlePath, dictDir, dictDirAbs :: FilePath
sylPath = testDir <> "stuff/tibetan-syllables"
titlePath = testDir <>  "stuff/titles.toml"
dictDir = testDir <> "dicts/"
dictDirAbs = home <> dictDir
