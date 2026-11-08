{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Env
    ( makeEnv
    , makeEnvFor
    , makeEnvFrom
    , Env (..)
    , readEnv
    , putEnvMVar
    , modifyEnv
    )
where

import Dictionary (DictionaryMeta, addAliases, makeDictionary, toDictionaryMeta)
import Label (LabelFull (..), Labels (..), getLabels)
import Type (HibetError (..))

import Control.Concurrent.MVar (MVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (IOException, try)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Paths_hibet (getDataFileName)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

-- | Environment fot translator
data Env = Env
    { dictionaryMeta :: ![DictionaryMeta]
    , labels :: !Labels
    }
    deriving stock (Eq, Generic)
    deriving anyclass (NFData)

makeEnv :: IO (Either HibetError Env)
makeEnv = makeEnvFor []

-- | Read only the dictionaries with the given ids; @[]@ means all of them.
makeEnvFor :: [Int] -> IO (Either HibetError Env)
makeEnvFor selected = do
    titlesPath <- getDataFileName "stuff/titles.toml"
    aliasesPath <- getDataFileName "stuff/key-aliases.tsv"
    dictsDir <- getDataFileName "dicts"
    makeEnvAt titlesPath aliasesPath dictsDir selected

-- | Read a data directory laid out like @hibet/data@: @stuff/titles.toml@,
-- @stuff/key-aliases.tsv@ and @dicts/@. Used by tests and by a server that
-- keeps the dictionaries outside the package.
makeEnvFrom :: FilePath -> [Int] -> IO (Either HibetError Env)
makeEnvFrom root selected =
    makeEnvAt
        (root </> "stuff" </> "titles.toml")
        (root </> "stuff" </> "key-aliases.tsv")
        (root </> "dicts")
        selected

makeEnvAt ::
    FilePath
    -> FilePath
    -> FilePath
    -> [Int]
    -> IO (Either HibetError Env)
makeEnvAt titlesPath aliasesPath dictsDir selected = do
    result <- try @IOException $ do
        labels@(Labels ls) <- getLabels <$> BS.readFile titlesPath
        aliases <- readAliases aliasesPath
        names <- listDirectory dictsDir
        let wanted file =
                null selected
                    || case find (\lf -> lf.path == T.pack (takeBaseName file)) ls of
                        Just lf -> lf.lfId `elem` selected
                        Nothing -> False
        filesAndTexts <-
            getFilesTexts (map (dictsDir </>) (filter wanted names))

        let dictsMeta =
                parMap
                    rdeepseq
                    ( \(f, t) ->
                        toDictionaryMeta ls f
                            $ addAliases aliases
                            $ makeDictionary
                            $ TL.toStrict t
                    )
                    filesAndTexts

        pure
            Env
                { dictionaryMeta = dictsMeta
                , labels = labels
                }
    pure $
        first
            (\e -> EffectError "Failed to load dictionaries" (T.pack (show e)))
            result

-- | Legacy key spelling and the canonical Wylie key a Unicode query gets for
-- the same word. The table is generated from the dictionaries; see
-- @scripts/gen-key-aliases.hs@.
readAliases :: FilePath -> IO (HashMap Text Text)
readAliases path = do
    text <- TE.decodeUtf8 <$> BS.readFile path
    let aliases =
            HMS.fromList
                [ (legacy, canonical)
                | line <- T.lines text
                , let (legacy, rest) = T.breakOn "\t" line
                , not (T.null legacy)
                , not (T.null rest)
                , let canonical = T.drop 1 rest
                , not (T.null canonical)
                ]
    HMS.size aliases `seq` pure aliases

getFilesTexts :: [FilePath] -> IO [(FilePath, TL.Text)]
getFilesTexts paths =
    zip paths <$> mapM (fmap TLE.decodeUtf8 . BSL.readFile) paths

readEnv :: MVar Env -> IO Env
readEnv = readMVar

modifyEnv :: MVar Env -> (Env -> IO Env) -> IO ()
modifyEnv = modifyMVar_

putEnvMVar :: MVar a -> a -> IO ()
putEnvMVar = putMVar
