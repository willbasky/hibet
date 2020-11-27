{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module App
  ( app
  , makeEnv
  )
  where

import Dictionary (makeTextMap, selectDict, toDictionaryMeta)
import Labels (getLabels)
import Parse
import Translator (loopDialog)
import Types

import Control.DeepSeq
import Control.Exception (bracketOnError)
import Control.Monad.Reader
import Control.Parallel.Strategies
import Data.Text (Text)
import Debug.Trace
import Path (fromAbsFile, parseAbsDir)
import Path.IO (listDir)
import Paths_hibet (getDataFileName)
import System.Console.Haskeline (defaultSettings)
import System.Console.Haskeline.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL




-- | Load environment and start loop dialog
app :: [Int] -> Hibet ()
app selectedDicts = do
  withReaderT (\env -> env{dictionaryMeta = selectDict selectedDicts env.dictionaryMeta}) $
    ReaderT $ \env -> do
      traceIO $ "ReaderT changed to: " <> show (length env.dictionaryMeta)
      bracketOnError
        (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such as a SigINT is received.
        (\inputState -> runReaderT (loopDialog inputState) env >> closeInput inputState)


-- Make environment
makeEnv :: IO Env
makeEnv = do
    sylsPath <- getDataFileName "stuff/tibetan-syllables"
    syls <- TE.decodeUtf8 <$> BS.readFile sylsPath
    labels@(Labels ls) <- getLabels <$> (BS.readFile =<< getDataFileName "stuff/titles.toml")
    (_, files) <- listDir =<< parseAbsDir =<< getDataFileName "dicts/"
    filesAndTexts <- sequence $ parMap rpar getFilesTexts files
    let dictsMeta = parMap (force rpar) (\(f,t) -> toDictionaryMeta ls f $ makeTextMap $ TL.toStrict t) filesAndTexts
    let env = Env
            { dictionaryMeta = dictsMeta
            , wylieTibet = syls `seq` withStrategy rpar $ makeWylieTibet syls
            , tibetWylie = syls `seq` withStrategy rpar $ makeTibetWylie syls
            , radixWylie = syls `seq` withStrategy rpar $ makeWylieRadexTree syls
            , radixTibet = syls `seq` withStrategy rpar $ makeTibetanRadexTree syls
            , labels     = labels `seq` withStrategy rpar $ labels
            }
    env `seq` pure env
  where
    -- getFilesTexts :: FilePath -> IO (FilePath, Text)
    getFilesTexts fp = do
      let path = fromAbsFile fp
      txt <- TLE.decodeUtf8 <$> BSL.readFile path
      pure (path, txt)

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
