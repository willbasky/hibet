{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Env
  ( makeEnv
  , Env(..)
  , updateEnv
  )
  where

import Dictionary (DictionaryMeta, makeDictionary, selectDict, toDictionaryMeta)
import Effects.File (FileIO)
import qualified Effects.File as File
import Label (Labels (..), getLabels)
import Parse (BimapWylieTibet, makeBi, makeTibetanRadexTree, makeWylieRadexTree, splitSyllables)
import Type (HibetError (..))

import Control.Monad.Except (runExcept)
import Control.Parallel.Strategies (NFData, parMap, rdeepseq, rparWith, runEval)
import Data.RadixTree (RadixTree)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Polysemy (Members, Sem)
import Polysemy.Error (Error, fromEither, throw)
import Polysemy.Path (Abs, File, Path, fromAbsFile)
import Polysemy.Trace (Trace, trace)


-- | Environment fot translator
data Env = Env
  { dictionaryMeta  :: ![DictionaryMeta]
  , bimapWylieTibet :: !BimapWylieTibet
  , radixWylie      :: !(RadixTree ())
  , radixTibet      :: !(RadixTree ())
  , labels          :: !Labels
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)


makeEnv :: Members [FileIO, Error HibetError, Trace] r => Sem r Env
makeEnv = do
    sylsPath <- File.getPath "stuff/tibetan-syllables"
    trace sylsPath

    syls <- TE.decodeUtf8 <$> File.readFile sylsPath
    labels@(Labels ls) <- getLabels <$> (File.readFile =<< File.getPath "stuff/titles.toml")

    dir <- File.getPath "dicts/"
    absDir <- File.parseAbsDir dir
    (_, files) <- File.listDirectory absDir
    filesAndTexts <- getFilesTexts files

    let dictsMeta = parMap (rparWith rdeepseq) (\(f,t) -> toDictionaryMeta ls f $ makeDictionary $ TL.toStrict t) filesAndTexts
    sylList <- fromEither $ runExcept $ splitSyllables syls
    pure $ runEval $ do
      wtSyllables <- rparWith rdeepseq $ makeBi sylList
      wRadix <- rparWith rdeepseq $ makeWylieRadexTree syls
      tRadix <- rparWith rdeepseq $ makeTibetanRadexTree syls
      pure Env
              { dictionaryMeta = dictsMeta
              , bimapWylieTibet = wtSyllables
              , radixWylie = wRadix
              , radixTibet = tRadix
              , labels     = labels
              }

getFilesTexts :: Members
  [ FileIO
  , Error HibetError] r
  => [Path Abs File] -> Sem r [(FilePath, TL.Text)]
getFilesTexts fp = do
  let paths = map fromAbsFile fp
  let contents = map (fmap TLE.decodeUtf8 . File.readFileLazy) paths
  txts <- sequenceA contents
  if length paths == length txts
    then pure $ zip paths txts
    else throw $ UnknownError "Not all dictionary files was read successfully"

updateEnv :: [Int] -> Env -> Env
updateEnv selectedDicts env =
  env{dictionaryMeta = selectDict selectedDicts env.dictionaryMeta}

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
