{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Env
  ( makeEnv
  , Env(..)
  )
  where

import Dictionary (DictionaryMeta, makeDictionary, toDictionaryMeta)
import Effects.File (FileIO)
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
import Polysemy (Members, Sem)
import Polysemy.Error (Error, fromEither, throw)
import Polysemy.Trace (Trace)

-- fo debug
-- import Polysemy.Trace (trace)
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


makeEnv :: Members [FileIO, Error HibetError, Trace] r => Sem r Env
makeEnv = do
    sylsPath <- File.getPath "stuff/tibetan-syllables"
    -- trace sylsPath

    syls <- TE.decodeUtf8 <$> File.readFile sylsPath
    labels@(Labels ls) <- getLabels <$> (File.readFile =<< File.getPath "stuff/titles.toml")

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
