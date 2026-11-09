module Main (main) where

import Fetch (newFetcher)
import KeyAliases (generateAliases)
import Manifest (checkManifest, defaultManifestVersion, writeManifest)
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
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> updateMode False
        ["--check"] -> updateMode True
        ["--gen-key-aliases"] -> do
            generateAliases "data/dicts" "data/key-aliases.tsv"
            refreshManifest
        ["--gen-manifest"] -> refreshManifest
        _ -> do
            hPutStrLn
                stderr
                "usage: hibet-update-dicts [--check | --gen-key-aliases | --gen-manifest]"
            exitFailure

refreshManifest :: IO ()
refreshManifest = do
    version <- defaultManifestVersion
    result <- writeManifest "data" version
    case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right count ->
            putStrLn
                ( "wrote data/manifest.toml: "
                    <> show count
                    <> " files, version "
                    <> T.unpack version
                )

updateMode :: Bool -> IO ()
updateMode checkOnly = do
    ref <- fromMaybe "master" <$> lookupEnv "REF"
    base <- fromMaybe (defaultBaseUrl ref) <$> lookupEnv "BASE_URL"
    putStrLn ("source: " <> base)
    putStrLn "target: data/dicts"
    fetcher <- newFetcher
    result <-
        runUpdate
            Config
                { dataRoot = "data"
                , baseUrl = base
                , checkOnly = checkOnly
                , fetch = fetcher
                }
            defaultEntries
    case result of
        Left err -> do
            hPutStrLn stderr err
            exitFailure
        Right stats -> do
            mapM_
                (\path -> hPutStrLn stderr ("no title in titles.toml: " <> path))
                stats.missingTitles
            printf
                "up to date: %d, changed: %d, added: %d\n"
                stats.same
                stats.changed
                stats.added
            manifestProblems <-
                if checkOnly then checkManifest "data" else pure []
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
                    putStrLn "regenerating data/key-aliases.tsv"
                    generateAliases "data/dicts" "data/key-aliases.tsv"
                    refreshManifest
