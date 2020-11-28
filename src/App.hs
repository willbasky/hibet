{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module App
  ( app
  , makeEnv
  )
  where

import Dictionary (makeTextMap, selectDict, toDictionaryMeta)
import Labels (getLabels)
import Parse
import Translator
import Types

-- import Control.DeepSeq
import Control.Exception (bracketOnError)
import Control.Monad.Reader
import Control.Parallel.Strategies
import Data.Text (Text)
-- import Debug.Trace
import Path (fromAbsFile, parseAbsDir)
import Path.IO (listDir)
import Paths_hibet (getDataFileName)
import System.Console.Haskeline (defaultSettings)
import System.Console.Haskeline.IO

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V




-- | Load environment and start loop dialog
app :: [Int] -> Hibet ()
app selectedDicts = do
  withReaderT (\env -> env{dictionaryMeta = selectDict selectedDicts env.dictionaryMeta}) $
    ReaderT $ \env -> do
      bracketOnError
        (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such as a SigINT is received.
        (\inputState -> runReaderT (loopDialog inputState) env >> closeInput inputState)
        -- (\inputState -> runReaderT (testDialog inputState) env >> closeInput inputState) -- for tests


-- Make environment
makeEnv :: IO Env
makeEnv = do
  sylsPath <- getDataFileName "stuff/tibetan-syllables"
  syls <- TE.decodeUtf8 <$> BS.readFile sylsPath
  labels@(Labels ls) <- getLabels <$> (BS.readFile =<< getDataFileName "stuff/titles.toml")
  (_, files) <- listDir =<< parseAbsDir =<< getDataFileName "dicts/"
  filesAndTexts <- V.fromList <$> traverse getFilesTexts files
  let dictsMeta = V.map (mkDictionaryMeta ls) filesAndTexts `using` parTraversable rdeepseq
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
  where
    getFilesTexts fp = do
      let path = fromAbsFile fp
      txt <- TE.decodeUtf8 <$> BS.readFile path
      pure (path, txt)

    mkDictionaryMeta :: [LabelFull] -> (FilePath, Text) -> DictionaryMeta
    mkDictionaryMeta ls (f,t) = toDictionaryMeta ls f $ makeTextMap t

