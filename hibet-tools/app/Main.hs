module Main (main) where

import Fetch (newFetcher)
import KeyAliases (generateAliases)
import Update
    ( Config (..)
    , Result (..)
    , defaultBaseUrl
    , defaultEntries
    , runUpdate
    )

import Control.Monad (when)
import Data.Maybe (fromMaybe)
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
        ["--gen-key-aliases"] -> generateAliases "data/dicts" "data/key-aliases.tsv"
        _ -> do
            hPutStrLn stderr "usage: hibet-update-dicts [--check | --gen-key-aliases]"
            exitFailure

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
            let dirty = stats.changed + stats.added > 0
                failed = not (null stats.missingTitles) || (checkOnly && dirty)
            if failed
                then exitFailure
                else when (not checkOnly && dirty) $ do
                    putStrLn "regenerating data/key-aliases.tsv"
                    generateAliases "data/dicts" "data/key-aliases.tsv"
