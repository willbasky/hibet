{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Env
    ( makeEnv
    , makeEnvFor
    , Env (..)
    , readEnv
    , putEnvMVar
    , modifyEnv
    )
where

import Dictionary (DictionaryMeta, addAliases, makeDictionary, toDictionaryMeta)
import Effects.File (FileSystem)
import qualified Effects.File as File
import Label (LabelFull (..), Labels (..), getLabels)
import Type (HibetError (..))

import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Path (Abs, File, Path, fromAbsFile)
import System.FilePath.Posix (takeBaseName)

import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (MVar, modifyMVar_, putMVar, readMVar)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Dynamic (Reader, ask)

-- | Environment fot translator
data Env = Env
    { dictionaryMeta :: ![DictionaryMeta]
    , labels :: !Labels
    }
    deriving stock (Eq, Generic)
    deriving anyclass (NFData)

makeEnv ::
    ( FileSystem :> es
    , Error HibetError :> es
    ) =>
    Eff es Env
makeEnv = makeEnvFor []

-- | Read only the dictionaries with the given ids; @[]@ means all of them.
makeEnvFor ::
    ( FileSystem :> es
    , Error HibetError :> es
    ) =>
    [Int]
    -> Eff es Env
makeEnvFor selected = do
    labels@(Labels ls) <-
        getLabels <$> (File.readFileBS =<< File.getPath "stuff/titles.toml")
    aliases <- readAliases

    dir <- File.getPath "dicts/"
    absDir <- File.parseAbsDir dir
    (_, files) <- File.listDirectory absDir
    let wanted file =
            null selected
                || case find (\lf -> lf.path == T.pack (takeBaseName (fromAbsFile file))) ls of
                    Just lf -> lf.lfId `elem` selected
                    Nothing -> False
    filesAndTexts <- getFilesTexts (filter wanted files)

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

-- | Legacy key spelling and the canonical Wylie key a Unicode query gets for
-- the same word. The table is generated from the dictionaries; see
-- @scripts/gen-key-aliases.hs@.
readAliases ::
    (FileSystem :> es) =>
    Eff es (HashMap T.Text T.Text)
readAliases = do
    text <-
        TE.decodeUtf8 <$> (File.readFileBS =<< File.getPath "stuff/key-aliases.tsv")
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

getFilesTexts ::
    ( FileSystem :> es
    , Error HibetError :> es
    ) =>
    [Path Abs File]
    -> Eff es [(FilePath, TL.Text)]
getFilesTexts fp = do
    let paths = map fromAbsFile fp
    let contents = map (fmap TLE.decodeUtf8 . File.readFileLazyBS) paths
    txts <- sequenceA contents
    if length paths == length txts
        then pure $ zip paths txts
        else throwError $ UnknownError "Some dictionary files fails to be read"

readEnv :: (Reader (MVar Env) :> es, Concurrent :> es) => Eff es Env
readEnv = do
    env <- ask
    readMVar env

modifyEnv ::
    (Reader (MVar Env) :> es, Concurrent :> es) => (Env -> Eff es Env) -> Eff es ()
modifyEnv f = do
    env <- ask
    modifyMVar_ env f

putEnvMVar :: (Reader (MVar a) :> es, Concurrent :> es) => a -> Eff es ()
putEnvMVar value = do
    env <- ask
    putMVar env value
