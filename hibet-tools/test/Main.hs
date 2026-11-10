module Main (main) where

import Fetch (newFetcher)
import KeyAliases (generateAliases)
import qualified Manifest
import Release (ReleaseInfo (..), buildRelease)
import Update
    ( Config (..)
    , Entry (..)
    , Result (..)
    , applyLocalFixes
    , normalizeDictionary
    , runUpdate
    )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
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
    manifestSpec
    releaseSpec

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

manifestSpec :: Spec
manifestSpec =
    describe "Manifest" $ do
        it "writes hashes in a fixed order and verifies them" $
            withSystemTempDirectory "hibet-manifest" $ \root -> do
                setupData root
                written <- Manifest.writeManifest root "2026.11.09"
                written `shouldBe` Right 3
                content <- TIO.readFile (root </> "manifest.toml")
                content
                    `shouldSatisfy` T.isInfixOf
                        "sha256 = \"92d944fe43bd6797e4ce6df996aab7b8243566737090d2e84790e539ea8fd78f\""
                manifest <- Manifest.buildManifest root "2026.11.09"
                map Manifest.name manifest.files
                    `shouldBe` ["dicts/A-T|E.txt", "titles.toml", "key-aliases.tsv"]
                Manifest.checkManifest root `shouldReturn` []
        it "reports a changed file" $
            withSystemTempDirectory "hibet-manifest" $ \root -> do
                setupData root
                _ <- Manifest.writeManifest root "2026.11.09"
                BS.writeFile (root </> "dicts" </> "A-T|E.txt") "re ba|hopE\n"
                Manifest.checkManifest root
                    `shouldReturn` ["sha256 mismatch: dicts/A-T|E.txt"]
        it "reports a missing file" $
            withSystemTempDirectory "hibet-manifest" $ \root -> do
                setupData root
                _ <- Manifest.writeManifest root "2026.11.09"
                removeFile (root </> "dicts" </> "A-T|E.txt")
                Manifest.checkManifest root
                    `shouldReturn` ["missing: dicts/A-T|E.txt"]
        it "reports a file that is not in the manifest" $
            withSystemTempDirectory "hibet-manifest" $ \root -> do
                setupData root
                _ <- Manifest.writeManifest root "2026.11.09"
                BS.writeFile (root </> "dicts" </> "B-T|E.txt") "ri ma|land\n"
                Manifest.checkManifest root
                    `shouldReturn` ["not in manifest: dicts/B-T|E.txt"]
        it "reports a missing manifest" $
            withSystemTempDirectory "hibet-manifest" $ \root -> do
                problems <- Manifest.checkManifest root
                problems
                    `shouldSatisfy` ( \case
                                        [problem] ->
                                            "manifest.toml" `T.isInfixOf` problem
                                        _ -> False
                                    )

releaseSpec :: Spec
releaseSpec =
    describe "Release" $ do
        it "builds manifest.toml and a tar.gz with the data files" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let root = tmp </> "data"
                    out = tmp </> "dist"
                setupData root
                _ <- Manifest.writeManifest root "2026.11.09"
                built <- buildRelease root out
                case built of
                    Left err -> expectationFailure err
                    Right info -> do
                        info.releaseVersion `shouldBe` "2026.11.09"
                        info.releaseFiles `shouldBe` 3
                        info.releaseArchive
                            `shouldBe` out </> "hibet-data-2026.11.09.tar.gz"
                        info.releaseManifest
                            `shouldBe` out </> "manifest.toml"
                        doesFileExist info.releaseArchive `shouldReturn` True
                        doesFileExist info.releaseManifest `shouldReturn` True
                        names <- archiveEntries info.releaseArchive
                        names
                            `shouldBe` [ "manifest.toml"
                                       , "dicts/A-T|E.txt"
                                       , "titles.toml"
                                       , "key-aliases.tsv"
                                       ]
        it "refuses to build when the data does not match the manifest" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let root = tmp </> "data"
                    out = tmp </> "dist"
                setupData root
                _ <- Manifest.writeManifest root "2026.11.09"
                BS.writeFile (root </> "dicts" </> "A-T|E.txt") "re ba|hopE\n"
                built <- buildRelease root out
                built `shouldSatisfy` isLeftWith "sha256 mismatch"
                doesFileExist (out </> "hibet-data-2026.11.09.tar.gz")
                    `shouldReturn` False

archiveEntries :: FilePath -> IO [FilePath]
archiveEntries path = do
    bytes <- BS.readFile path
    pure (go (Tar.read (GZip.decompress (BSL.fromStrict bytes))))
    where
        go = \case
            Tar.Fail err -> error (show err)
            Tar.Done -> []
            Tar.Next entry rest -> Tar.entryPath entry : go rest

setupData :: FilePath -> IO ()
setupData root = do
    createDirectoryIfMissing True (root </> "dicts")
    BS.writeFile (root </> "dicts" </> "A-T|E.txt") "re ba|hope\n"
    BS.writeFile (root </> "titles.toml") "[[titles]]\n"
    BS.writeFile (root </> "key-aliases.tsv") "bha ga\tb+ha ga\n"

isLeftWith :: String -> Either String a -> Bool
isLeftWith needle = \case
    Left err -> needle `isInfixOf` err
    Right _ -> False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
