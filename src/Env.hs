{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE BangPatterns #-}

module Env
  ( makeEnv
  , Env(..)
  , readEnv
  , putEnvMVar
  , modifyEnv
  )
  where

import Dictionary (DictionaryMeta, makeDictionary, toDictionaryMeta)
import Effects.File (FileSystem)
import qualified Effects.File as File
import Label (Labels (..), getLabels)
import Parse (TibetWylieMap, WylieTibetMap, mkTibetanRadex, mkWylieRadex, splitSyllables)
import Type (HibetError (..))

import Control.Parallel.Strategies (NFData, parMap, rdeepseq, rparWith, runEval)
import qualified Data.HashMap.Strict as HM
import Data.RadixTree (RadixTree)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Path (Abs, File, Path, fromAbsFile)

import Effectful ( type (:>), Eff )
import Effectful.Error.Static ( Error, throwError )
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Dynamic (Reader)
import Effectful.Concurrent.MVar (MVar, modifyMVar_, readMVar, putMVar)
import Effectful.Reader.Dynamic (ask)

-- fo debug
-- import qualified Data.Bimap as Bi
-- import Parse (WylieSyllable(WylieSyllable))


-- | Environment fot translator
data Env = Env
  { dictionaryMeta :: ![DictionaryMeta]
  , wylieTibetMap  :: !WylieTibetMap
  , tibetWylieMap  :: !TibetWylieMap
  , radixWylie     :: !(RadixTree ())
  , radixTibet     :: !(RadixTree ())
  , labels         :: !Labels
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)


makeEnv ::
  (  FileSystem :> es
  ,  Error HibetError :> es
  )
  => Eff es Env
makeEnv = do
    sylsPath <- File.getPath "stuff/tibetan-syllables"
    -- trace sylsPath

    syls <- TE.decodeUtf8 <$> File.readFileBS sylsPath
    labels@(Labels ls) <- getLabels <$> (File.readFileBS =<< File.getPath "stuff/titles.toml")

    dir <- File.getPath "dicts/"
    absDir <- File.parseAbsDir dir
    (_, files) <- File.listDirectory absDir
    filesAndTexts <- getFilesTexts files

    let dictsMeta = parMap (rparWith rdeepseq)
          (\(f,t) -> toDictionaryMeta ls f $ makeDictionary $ TL.toStrict t) filesAndTexts

    sylList <- fromEither $ splitSyllables syls
    pure $ runEval $ do
          wtMap <- rparWith rdeepseq $ HM.fromList sylList
          twMap <- rparWith rdeepseq $ HM.fromList $ map swap sylList
          wRadix <- rparWith rdeepseq $ mkWylieRadex syls
          tRadix <- rparWith rdeepseq $ mkTibetanRadex syls
          pure Env
              { dictionaryMeta = dictsMeta
              , wylieTibetMap = wtMap
              , tibetWylieMap = twMap
              , radixWylie = wRadix
              , radixTibet = tRadix
              , labels     = labels
              }

getFilesTexts ::
  (  FileSystem :> es
  ,  Error HibetError :> es
  )
  => [Path Abs File]
  -> Eff es [(FilePath, TL.Text)]
getFilesTexts fp = do
  let paths = map fromAbsFile fp
  let contents = map (fmap TLE.decodeUtf8 . File.readFileLazyBS) paths
  txts <- sequenceA contents
  if length paths == length txts
    then pure $ zip paths txts
    else throwError $ UnknownError "Some dictionary files fails to be read"

fromEither :: (Error HibetError :> es)
  => Either HibetError a
  -> Eff es a
fromEither (Left err) = throwError err
fromEither (Right res) = pure res

readEnv :: (Reader (MVar Env) :> es, Concurrent :> es) => Eff es Env
readEnv = do
  env <- ask
  readMVar env

modifyEnv :: (Reader (MVar Env) :> es, Concurrent :> es) => (Env -> Eff es Env) -> Eff es ()
modifyEnv f = do
  env <- ask
  modifyMVar_ env f

putEnvMVar :: (Reader (MVar a) :> es, Concurrent :> es) => a -> Eff es ()
putEnvMVar value = do
  env <- ask
  putMVar env value

    -- getFilesTextsPar fs = mapM (\f -> do
    --   let path = fromAbsFile f
    --   txt <- TE.decodeUtf8 <$> BS.readFile path
    --   pure (path, txt)) fs

-- parMapC :: (a -> b) -> [a] -> Eval [b]
-- parMapC f [] = return []
-- parMapC f (a:as) = do
--    b <- rpar (f a)
--    bs <- parMapC f as
--    return (b:bs)

-- parMap strat f = withStrategy (parList strat) . map f

-- parTraverseC :: Applicative t1 => Strategy [b] -> (a -> t1 b) -> [a] -> t1 [b]
-- parTraverseC strat f = withStrategy (parTraversable strat) . traverse f
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)

