{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Env
  ( makeEnv
  )
  where

import Dictionary (makeTextMap, toDictionaryMeta)
import Effects.File (FileIO, runFile)
import qualified Effects.File as File
import Labels (getLabels)
import Parse
import Types

import Control.Parallel.Strategies
import Data.Function ((&))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Polysemy (Members, Sem, runM)
import Polysemy.Path (Path, Abs, File, fromAbsFile)
import Polysemy.Error (Error, throw, runError)


-- Make environment
makeEnv :: IO Environment
makeEnv = runEnv makeEnvS

runEnv :: (forall r . Members [FileIO, Error HibetErrors] r => Sem r Env)
  -> IO Environment
runEnv program = program
  & runFile
  & runError @HibetErrors
  & runM

makeEnvS :: Members [FileIO, Error HibetErrors] r => Sem r Env
makeEnvS = do
    sylsPath <- File.getPath "stuff/tibetan-syllables"
    syls <- TE.decodeUtf8 <$> File.readFile sylsPath
    labels@(Labels ls) <- getLabels <$> (File.readFile =<< File.getPath "stuff/titles.toml")
    absDir <- File.parseAbsDirectory =<< File.getPath "dicts/"
    (_, files) <- File.listDirectory absDir
    filesAndTexts <- getFilesTexts files
    let dictsMeta = parMap (rparWith rdeepseq) (\(f,t) -> toDictionaryMeta ls f $ makeTextMap $ TL.toStrict t) filesAndTexts
    pure $ runEval $ do
      wt <- rparWith rdeepseq $ makeWylieTibet syls
      tw <- rparWith rdeepseq $ makeTibetWylie syls
      wr <- rparWith rdeepseq $ makeWylieRadexTree syls
      tr <- rparWith rdeepseq $ makeTibetanRadexTree syls
      pure Env
              { dictionaryMeta = dictsMeta
              , wylieTibet = wt
              , tibetWylie = tw
              , radixWylie = wr
              , radixTibet = tr
              , labels     = labels
              }

getFilesTexts :: Members
  [ FileIO
  , Error HibetErrors] r
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