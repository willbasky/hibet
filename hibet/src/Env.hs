{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Env
    ( makeEnv
    , makeEnvFor
    , makeEnvForDataDir
    , makeEnvFrom
    , resolveDataDir
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
import Control.Concurrent.STM (atomically)
import Control.DeepSeq (force)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (filterM)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Ki
import System.Directory
    ( XdgDirectory (XdgData)
    , doesDirectoryExist
    , doesFileExist
    , getXdgDirectory
    , listDirectory
    )
import System.Environment (lookupEnv)
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
makeEnvFor = makeEnvForDataDir Nothing

-- | Like 'makeEnvFor', but an explicit data root wins over the environment.
makeEnvForDataDir :: Maybe FilePath -> [Int] -> IO (Either HibetError Env)
makeEnvForDataDir dir selected = do
    resolved <- resolveDataDir dir
    case resolved of
        Left err -> pure (Left err)
        Right root -> makeEnvFrom root selected

-- | Resolve the data root: @--data-dir@, then @HIBET_DATA_DIR@, then
-- @$XDG_DATA_HOME/hibet/data@. A candidate is accepted only if it holds
-- @titles.toml@ and @dicts/@.
resolveDataDir :: Maybe FilePath -> IO (Either HibetError FilePath)
resolveDataDir explicit = do
    candidates <- case explicit of
        Just root -> pure [root]
        Nothing -> do
            envRoot <- lookupEnv "HIBET_DATA_DIR"
            xdgRoot <- getXdgDirectory XdgData "hibet"
            pure $ maybe [] (: []) (envRoot >>= nonEmpty) <> [xdgRoot </> "data"]
    existing <- filterM hasData candidates
    pure $ case existing of
        root : _ -> Right root
        [] -> Left $ noData candidates
    where
        nonEmpty "" = Nothing
        nonEmpty root = Just root
        hasData root = do
            titles <- doesFileExist (root </> "titles.toml")
            dicts <- doesDirectoryExist (root </> "dicts")
            pure (titles && dicts)
        noData candidates =
            EffectError "Dictionary data not found" $
                "looked in: "
                    <> T.intercalate ", " (map T.pack candidates)
                    <> ". Install it with `hibet data install --from DIR`"
                    <> " or `hibet data update`, or pass --data-dir=DIR /"
                    <> " set HIBET_DATA_DIR to a data root with titles.toml"
                    <> " and dicts/."

-- | Read a data directory laid out like @hibet-data/@: @titles.toml@,
-- @key-aliases.tsv@ and @dicts/@. Used by tests and by a server that keeps
-- the dictionaries outside the package. The returned environment is fully
-- evaluated: every selected dictionary is parsed before it is returned.
makeEnvFrom :: FilePath -> [Int] -> IO (Either HibetError Env)
makeEnvFrom root selected =
    makeEnvAt
        (root </> "titles.toml")
        (root </> "key-aliases.tsv")
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

        let env =
                Env
                    { dictionaryMeta =
                        parMap
                            rdeepseq
                            ( \(f, t) ->
                                toDictionaryMeta ls f
                                    $ addAliases aliases
                                    $ makeDictionary t
                            )
                            filesAndTexts
                    , labels = labels
                    }
        -- The CLI loads the environment in a background thread: parse the
        -- dictionaries here, not in the first query that reads the MVar.
        _ <- evaluate (force env)
        pure env
    pure $
        first
            (\e -> EffectError "Failed to load dictionaries" (T.pack (show e)))
            result

-- | Legacy key spelling and the canonical Wylie key a Unicode query gets for
-- the same word. The table is generated from the dictionaries; see
-- @hibet-dataset generate-key-aliases@.
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

-- | Read the files strictly, one green thread per file. In T8 this beat lazy
-- bytestring reads under 'parMap', and strict 'Text' allocated about 13% less.
getFilesTexts :: [FilePath] -> IO [(FilePath, Text)]
getFilesTexts paths =
    Ki.scoped $ \scope -> do
        threads <-
            traverse
                (\path -> Ki.fork scope ((path,) . TE.decodeUtf8 <$> BS.readFile path))
                paths
        atomically (traverse Ki.await threads)

readEnv :: MVar Env -> IO Env
readEnv = readMVar

modifyEnv :: MVar Env -> (Env -> IO Env) -> IO ()
modifyEnv = modifyMVar_

putEnvMVar :: MVar a -> a -> IO ()
putEnvMVar = putMVar
