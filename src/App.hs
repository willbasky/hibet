{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module App
  ( app
  , makeEnv
  )
  where

import Labels (getLabels)
import Parse
import Dictionary (makeTextMap, selectDict, toDictionaryMeta)
import Types
import Translator (loopDialog)

import Control.Exception (bracketOnError)
import Control.Monad.Reader
import Path (fromAbsFile, parseAbsDir)
import Path.IO (listDir)
import Paths_hibet (getDataFileName)
import System.Console.Haskeline (defaultSettings)
import System.Console.Haskeline.IO

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE




-- | Load environment and start loop dialog
app :: [Int] -> Hibet ()
app selectedDicts = do
  withReaderT (\env -> env{dictionaryMeta = selectDict selectedDicts env.dictionaryMeta}) $
    ReaderT $ \env -> bracketOnError
      (initializeInput defaultSettings)
      cancelInput -- This will only be called if an exception such as a SigINT is received.
      (\inputState -> runReaderT (loopDialog inputState) env >> closeInput inputState)


-- Make environment
makeEnv :: IO Env
makeEnv = do
    sylsPath <- getDataFileName "stuff/tibetan-syllables"
    syls <- TE.decodeUtf8 <$> BS.readFile sylsPath
    labels@(Labels ls) <- getLabels <$> (BS.readFile =<< (getDataFileName "stuff/titles.toml"))
    (_, files) <- listDir =<< parseAbsDir "dicts/"
    filesAndTexts <- traverse getFilesTexts files
    let dictsMeta = map (\(f,t) -> toDictionaryMeta ls f $ makeTextMap t) filesAndTexts
    pure Env
            -- { dictionaryMeta = selectDict selectedIds dictsMeta
            { dictionaryMeta = dictsMeta
            , wylieTibet = makeWylieTibet syls
            , tibetWylie = makeTibetWylie syls
            , radixWylie = makeWylieRadexTree syls
            , radixTibet = makeTibetanRadexTree syls
            , labels     = labels
            }
  where
    getFilesTexts fp = do
      let path = fromAbsFile fp
      txt <- TE.decodeUtf8 <$> BS.readFile path
      pure (path, txt)

