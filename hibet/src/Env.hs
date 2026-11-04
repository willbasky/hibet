{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Env
    ( makeEnv
    , Env (..)
    , readEnv
    , putEnvMVar
    , modifyEnv
    )
where

import Dictionary (DictionaryMeta, makeDictionary, toDictionaryMeta)
import Effects.File (FileSystem)
import qualified Effects.File as File
import Label (Labels (..), getLabels)
import Type (HibetError (..))

import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Path (Abs, File, Path, fromAbsFile)

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
makeEnv = do
    labels@(Labels ls) <-
        getLabels <$> (File.readFileBS =<< File.getPath "stuff/titles.toml")

    dir <- File.getPath "dicts/"
    absDir <- File.parseAbsDir dir
    (_, files) <- File.listDirectory absDir
    filesAndTexts <- getFilesTexts files

    let dictsMeta =
            parMap
                rdeepseq
                (\(f, t) -> toDictionaryMeta ls f $ makeDictionary $ TL.toStrict t)
                filesAndTexts

    pure
        Env
            { dictionaryMeta = dictsMeta
            , labels = labels
            }

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
