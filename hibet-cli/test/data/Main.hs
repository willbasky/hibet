module Main (main) where

import Install
    ( Installed (..)
    , installFromDir
    , installFromRelease
    )
import Manifest (Manifest (..), parseManifest, verifyDir)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tare
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    )
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
    installSpec
    releaseSpec
    manifestSpec

installSpec :: Spec
installSpec =
    describe "install --from DIR" $ do
        it "installs a verified directory" $
            withSystemTempDirectory "hibet-install" $ \tmp -> do
                let source = tmp </> "source"
                    target = tmp </> "hibet"
                writeDataDir source
                result <- installFromDir source target
                case result of
                    Left err -> expectationFailure err
                    Right installed -> do
                        installed.installedVersion `shouldBe` "2026.11.09"
                        installed.installedFiles `shouldBe` 3
                        installed.installedPath `shouldBe` target </> "data"
                        TIO.readFile
                            (target </> "data" </> "dicts" </> "A-T|E.txt")
                            `shouldReturn` "re ba|hope\n"
                        doesFileExist (target </> "data" </> "manifest.toml")
                            `shouldReturn` True
        it "keeps the previous set in data.old" $
            withSystemTempDirectory "hibet-install" $ \tmp -> do
                let source = tmp </> "source"
                    target = tmp </> "hibet"
                writeDataDir source
                _ <- installFromDir source target
                doesDirectoryExist (target </> "data.old")
                    `shouldReturn` False
                _ <- installFromDir source target
                doesDirectoryExist (target </> "data.old")
                    `shouldReturn` True
        it "rejects a changed source file" $
            withSystemTempDirectory "hibet-install" $ \tmp -> do
                let source = tmp </> "source"
                    target = tmp </> "hibet"
                writeDataDir source
                BS.writeFile
                    (source </> "dicts" </> "A-T|E.txt")
                    "re ba|hopE\n"
                result <- installFromDir source target
                result `shouldSatisfy` isLeftWith "sha256 mismatch"
                doesDirectoryExist (target </> "data") `shouldReturn` False
        it "reports a missing manifest" $
            withSystemTempDirectory "hibet-install" $ \tmp -> do
                result <- installFromDir (tmp </> "empty") (tmp </> "hibet")
                result `shouldSatisfy` isLeftWith "missing"
        it "reports a manifest without dicts/" $
            withSystemTempDirectory "hibet-install" $ \tmp -> do
                let source = tmp </> "source"
                createDirectoryIfMissing True source
                TIO.writeFile (source </> "manifest.toml") manifestText
                result <- installFromDir source (tmp </> "hibet")
                result `shouldSatisfy` isLeftWith "cannot list"

releaseSpec :: Spec
releaseSpec =
    describe "update --source" $ do
        it "installs an archive from a file:// release" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let release = tmp </> "release"
                    target = tmp </> "hibet"
                writeDataDir release
                writeArchive
                    (release </> "hibet-data-2026.11.09.tar.gz")
                    dataFiles
                result <-
                    installFromRelease
                        ("file://" <> release)
                        target
                case result of
                    Left err -> expectationFailure err
                    Right installed -> do
                        installed.installedVersion `shouldBe` "2026.11.09"
                        TIO.readFile
                            (target </> "data" </> "dicts" </> "A-T|E.txt")
                            `shouldReturn` "re ba|hope\n"
        it "installs from a plain directory path" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let release = tmp </> "release"
                    target = tmp </> "hibet"
                writeDataDir release
                writeArchive
                    (release </> "hibet-data-2026.11.09.tar.gz")
                    dataFiles
                result <- installFromRelease release target
                result `shouldSatisfy` isRight
        it "rejects an archive whose payload does not match the manifest" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let release = tmp </> "release"
                    target = tmp </> "hibet"
                writeDataDir release
                writeArchive
                    (release </> "hibet-data-2026.11.09.tar.gz")
                    (replacePayload dataFiles)
                result <- installFromRelease release target
                result `shouldSatisfy` isLeftWith "sha256 mismatch"
                doesDirectoryExist (target </> "data") `shouldReturn` False
        it "rejects an unexpected file in the archive" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let release = tmp </> "release"
                    target = tmp </> "hibet"
                writeDataDir release
                writeArchive
                    (release </> "hibet-data-2026.11.09.tar.gz")
                    (("evil.txt", "junk") : dataFiles)
                result <- installFromRelease release target
                result `shouldSatisfy` isLeftWith "unexpected archive file"
        it "rejects a path that escapes the staging directory" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let release = tmp </> "release"
                    target = tmp </> "hibet"
                writeDataDir release
                writeArchive
                    (release </> "hibet-data-2026.11.09.tar.gz")
                    (("../evil.txt", "junk") : dataFiles)
                result <- installFromRelease release target
                result `shouldSatisfy` isLeftWith "unsafe archive path"
        it "reports a missing release asset" $
            withSystemTempDirectory "hibet-release" $ \tmp -> do
                let release = tmp </> "release"
                createDirectoryIfMissing True release
                TIO.writeFile (release </> "manifest.toml") manifestText
                result <- installFromRelease release (tmp </> "hibet")
                result `shouldSatisfy` isLeftWith "tar.gz"

manifestSpec :: Spec
manifestSpec =
    describe "manifest" $ do
        it "parses a schema 1 manifest" $
            parseManifest manifestText
                `shouldSatisfy` ( \case
                                    Right manifest ->
                                        length manifest.files == 3
                                    Left _ -> False
                                )
        it "rejects a path outside the data root" $
            parseManifest (T.replace "dicts/A-T|E.txt" "../evil" manifestText)
                `shouldSatisfy` isLeftWith "unsafe path"
        it "reports a missing data file" $
            withSystemTempDirectory "hibet-manifest" $ \tmp -> do
                createDirectoryIfMissing True (tmp </> "dicts")
                case parseManifest manifestText of
                    Left err -> expectationFailure err
                    Right manifest -> do
                        problems <- verifyDir tmp manifest
                        problems
                            `shouldSatisfy` any
                                (T.isInfixOf "missing: dicts/A-T|E.txt")

manifestText :: T.Text
manifestText =
    T.unlines
        [ "schema = 1"
        , "version = \"2026.11.09\""
        , ""
        , "[[files]]"
        , "name = \"dicts/A-T|E.txt\""
        , "sha256 = \"92d944fe43bd6797e4ce6df996aab7b8243566737090d2e84790e539ea8fd78f\""
        , "size = 11"
        , ""
        , "[[files]]"
        , "name = \"titles.toml\""
        , "sha256 = \"f9a42e1d86a3e0e6c9b2080eae9c4c7083af640adc5d2e04359f8dc95d30c1eb\""
        , "size = 11"
        , ""
        , "[[files]]"
        , "name = \"key-aliases.tsv\""
        , "sha256 = \"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\""
        , "size = 0"
        ]

writeDataDir :: FilePath -> IO ()
writeDataDir root = do
    createDirectoryIfMissing True (root </> "dicts")
    BS.writeFile (root </> "dicts" </> "A-T|E.txt") "re ba|hope\n"
    BS.writeFile (root </> "titles.toml") "[[titles]]\n"
    BS.writeFile (root </> "key-aliases.tsv") ""
    TIO.writeFile (root </> "manifest.toml") manifestText

dataFiles :: [(FilePath, BS.ByteString)]
dataFiles =
    [ ("dicts/A-T|E.txt", "re ba|hope\n")
    , ("titles.toml", "[[titles]]\n")
    , ("key-aliases.tsv", "")
    ]

replacePayload :: [(FilePath, BS.ByteString)] -> [(FilePath, BS.ByteString)]
replacePayload =
    map $ \case
        ("dicts/A-T|E.txt", _) -> ("dicts/A-T|E.txt", "re ba|hopE\n")
        entry -> entry

writeArchive :: FilePath -> [(FilePath, BS.ByteString)] -> IO ()
writeArchive path files =
    BSL.writeFile path (GZip.compress (Tar.write (map entry files)))
    where
        entry (name, content) =
            case Tare.toTarPath False name of
                Left err -> error err
                Right tarPath -> Tare.fileEntry tarPath (BSL.fromStrict content)

isLeftWith :: String -> Either String a -> Bool
isLeftWith needle = \case
    Left err -> needle `isInfixOf` err
    Right _ -> False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False
