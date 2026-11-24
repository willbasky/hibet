module Main (main) where

import Fetch (newFetcher)
import KeyAliases (generateAliases)
import Manifest
    ( checkManifest
    , defaultManifestVersion
    , renderManifestError
    , writeManifest
    )
import Release (ReleaseInfo (..), buildRelease)
import Update
    ( Config (..)
    , Result (..)
    , defaultBaseUrl
    , defaultEntries
    , runUpdate
    )

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , command
    , customExecParser
    , fullDesc
    , help
    , helper
    , info
    , metavar
    , optional
    , prefs
    , progDesc
    , showHelpOnEmpty
    , str
    , subparser
    )
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import Text.Printf (printf)

datasetDir :: FilePath
datasetDir = "hibet-data"

data Command
    = UpdateData
    | CheckData
    | CheckManifest
    | GenerateKeyAliases
    | GenerateManifest
    | Release (Maybe FilePath)

main :: IO ()
main = do
    selected <- customExecParser (prefs showHelpOnEmpty) commandInfo
    case selected of
        UpdateData -> updateMode False
        CheckData -> updateMode True
        CheckManifest -> checkManifestMode
        GenerateKeyAliases -> do
            generateAliases
                (datasetDir </> "dicts")
                (datasetDir </> "key-aliases.tsv")
            refreshManifest
        GenerateManifest -> refreshManifest
        Release mdir -> releaseMode (fromMaybe "dist/data-release" mdir)

commandInfo :: ParserInfo Command
commandInfo =
    info
        (helper <*> commandP)
        (fullDesc <> progDesc "Maintain hibet dictionary data")

commandP :: Parser Command
commandP =
    subparser $
        command
            "update"
            ( info
                (helper <*> pure UpdateData)
                $ progDesc "Download upstream dictionaries and update hibet-data/dicts"
            )
            <> command
                "check"
                ( info
                    (helper <*> pure CheckData)
                    $ progDesc "Compare hibet-data/dicts with upstream and verify the manifest"
                )
            <> command
                "check-manifest"
                ( info
                    (helper <*> pure CheckManifest)
                    $ progDesc "Verify hibet-data/manifest.toml against hibet-data/ without network"
                )
            <> command
                "generate-key-aliases"
                ( info
                    (helper <*> pure GenerateKeyAliases)
                    $ progDesc "Rebuild hibet-data/key-aliases.tsv and the manifest"
                )
            <> command
                "generate-manifest"
                ( info
                    (helper <*> pure GenerateManifest)
                    $ progDesc "Rebuild hibet-data/manifest.toml"
                )
            <> command
                "release"
                ( info
                    (helper <*> releaseP)
                    $ progDesc "Build the release archive"
                )
    where
        releaseP =
            Release
                <$> optional
                    ( argument
                        str
                        ( metavar "DIR"
                            <> help "Output directory (default: dist/data-release)"
                        )
                    )

releaseMode :: FilePath -> IO ()
releaseMode outDir = do
    built <- buildRelease datasetDir outDir
    case built of
        Left err -> TIO.hPutStrLn stderr ("release failed: " <> err) >> exitFailure
        Right result -> do
            TIO.putStrLn ("artifact: " <> T.pack result.releaseArchive)
            TIO.putStrLn ("manifest: " <> T.pack result.releaseManifest)
            TIO.putStrLn ("version: " <> result.releaseVersion)
            TIO.putStrLn (T.pack (show result.releaseFiles) <> " data files")

checkManifestMode :: IO ()
checkManifestMode = do
    problems <- checkManifest datasetDir
    mapM_ (TIO.hPutStrLn stderr . ("manifest: " <>)) problems
    if null problems
        then TIO.putStrLn "manifest: ok"
        else exitFailure

refreshManifest :: IO ()
refreshManifest = do
    version <- defaultManifestVersion
    result <- writeManifest datasetDir version
    case result of
        Left err -> TIO.hPutStrLn stderr (renderManifestError err) >> exitFailure
        Right count ->
            TIO.putStrLn
                ( "wrote hibet-data/manifest.toml: "
                    <> T.pack (show count)
                    <> " files, version "
                    <> version
                )

updateMode :: Bool -> IO ()
updateMode checkOnly = do
    ref <- fromMaybe "master" <$> lookupEnv "REF"
    base <- fromMaybe (defaultBaseUrl ref) <$> lookupEnv "BASE_URL"
    TIO.putStrLn ("source: " <> T.pack base)
    TIO.putStrLn "target: hibet-data/dicts"
    fetcher <- newFetcher
    result <-
        runUpdate
            Config
                { dataRoot = datasetDir
                , baseUrl = base
                , checkOnly = checkOnly
                , fetch = fetcher
                }
            defaultEntries
    case result of
        Left err -> do
            TIO.hPutStrLn stderr err
            exitFailure
        Right stats -> do
            mapM_
                (\path -> TIO.hPutStrLn stderr ("no title in titles.toml: " <> T.pack path))
                stats.missingTitles
            TIO.putStrLn
                ( T.pack
                    ( printf
                        "up to date: %d, changed: %d, added: %d"
                        stats.same
                        stats.changed
                        stats.added
                    )
                )
            manifestProblems <-
                if checkOnly then checkManifest datasetDir else pure []
            mapM_
                (TIO.hPutStrLn stderr . ("manifest: " <>))
                manifestProblems
            let dirty = stats.changed + stats.added > 0
                failed =
                    not (null stats.missingTitles)
                        || not (null manifestProblems)
                        || (checkOnly && dirty)
            if failed
                then exitFailure
                else when (not checkOnly && dirty) $ do
                    TIO.putStrLn "regenerating hibet-data/key-aliases.tsv"
                    generateAliases
                        (datasetDir </> "dicts")
                        (datasetDir </> "key-aliases.tsv")
                    refreshManifest
