module Main (main) where

import Fetch (newFetcher)
import KeyAliases (generateAliases)
import Update
    ( Config (..)
    , Entry (..)
    , Result (..)
    , applyLocalFixes
    , normalizeDictionary
    , runUpdate
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , hspec
    , it
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    )

main :: IO ()
main = hspec $ do
    normalizeSpec
    updateSpec
    aliasSpec

normalizeSpec :: Spec
normalizeSpec =
    describe "Dictionary normalization" $ do
        it "drops comments, blanks, bad lines and empty parts" $
            normalizeDictionary
                ( T.unlines
                    [ "# comment"
                    , ""
                    , "no separator"
                    , "|empty key"
                    , "re ba|"
                    , "re ba|hope"
                    , "re ba| hope "
                    ]
                )
                `shouldBe` "re ba|hope\nre ba| hope\n"
        it "trims trailing whitespace" $
            normalizeDictionary "re ba|hope   \n"
                `shouldBe` "re ba|hope\n"
        it "normalizes to NFC" $
            normalizeDictionary (T.pack ['e', '\x0301'] <> "|accent\n")
                `shouldBe` (T.pack ['\x00e9'] <> "|accent\n")
        it "fixes the Sera regression" $
            applyLocalFixes
                "Sera-Textbook-Def-T|T"
                "{yrongs grub|x}\n"
                `shouldBe` "{yongs grub|x}\n"
        it "fixes the JimValby regression" $
            applyLocalFixes
                "JimValby-T|E"
                "sne btags|   \nother|line\n"
                `shouldBe` "sne btags|tag\nother|line\n"

updateSpec :: Spec
updateSpec =
    describe "Dictionary update" $ do
        it "checks counters without writing and then writes in update mode" $
            withSystemTempDirectory "hibet-update" $ \tmp -> do
                (config, entries) <- setup tmp
                checked <- runUpdate config{checkOnly = True} entries
                checked
                    `shouldBe` Right
                        Result
                            { same = 0
                            , changed = 0
                            , added = 2
                            , missingTitles = []
                            }
                let dest = dataRoot config </> "dicts" </> "A-T|E.txt"
                doesFileExist dest `shouldReturn` False
                updated <- runUpdate config{checkOnly = False} entries
                updated
                    `shouldBe` Right
                        Result
                            { same = 0
                            , changed = 0
                            , added = 2
                            , missingTitles = []
                            }
                TIO.readFile dest `shouldReturn` "re ba|hope\n"
                again <- runUpdate config{checkOnly = False} entries
                again
                    `shouldBe` Right
                        Result
                            { same = 2
                            , changed = 0
                            , added = 0
                            , missingTitles = []
                            }
        it "reports a changed file" $
            withSystemTempDirectory "hibet-update" $ \tmp -> do
                (config, entries) <- setup tmp
                _ <- runUpdate config{checkOnly = False} entries
                BS.writeFile (tmp </> "upstream" </> "a.txt") "re ba|expectation\n"
                checked <- runUpdate config{checkOnly = True} entries
                checked
                    `shouldBe` Right
                        Result
                            { same = 1
                            , changed = 1
                            , added = 0
                            , missingTitles = []
                            }
        it "reports a missing title" $
            withSystemTempDirectory "hibet-update" $ \tmp -> do
                (config, entries) <- setup tmp
                TIO.writeFile
                    (dataRoot config </> "titles.toml")
                    "[[titles]]\npath = \"A-T|E\"\n"
                checked <- runUpdate config{checkOnly = True} entries
                checked
                    `shouldBe` Right
                        Result
                            { same = 0
                            , changed = 0
                            , added = 2
                            , missingTitles = ["B-T|E"]
                            }
        it "fails on an unreadable upstream file" $
            withSystemTempDirectory "hibet-update" $ \tmp -> do
                (config, entries) <- setup tmp
                failed <- runUpdate config{baseUrl = "file://" <> tmp </> "none"} entries
                failed `shouldSatisfy` isLeft
    where
        setup tmp = do
            let upstream = tmp </> "upstream"
                root = tmp </> "data"
            createDirectoryIfMissing True upstream
            createDirectoryIfMissing True (root </> "dicts")
            BS.writeFile (upstream </> "a.txt") "re ba|hope\n"
            BS.writeFile (upstream </> "b.txt") "ri ma|land\n"
            TIO.writeFile
                (root </> "titles.toml")
                "[[titles]]\npath = \"A-T|E\"\n\n[[titles]]\npath = \"B-T|E\"\n"
            fetcher <- newFetcher
            pure
                ( Config
                    { dataRoot = root
                    , baseUrl = "file://" <> upstream
                    , checkOnly = True
                    , fetch = fetcher
                    }
                , [Entry "a.txt" "A-T|E", Entry "b.txt" "B-T|E"]
                )

aliasSpec :: Spec
aliasSpec =
    describe "Key aliases" $
        it "writes a faithful legacy-to-canonical pair" $
            withSystemTempDirectory "hibet-aliases" $ \tmp -> do
                let out = tmp </> "key-aliases.tsv"
                generateAliases "test/data/dicts" out
                content <- TIO.readFile out
                content
                    `shouldSatisfy` T.isInfixOf "bha ga\tb+ha ga"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
