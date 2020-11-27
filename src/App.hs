module App
  ( app
  )
  where


import Control.Exception (bracketOnError)
-- import Control.Monad (forever)
import Control.Monad.Except
import Data.List (foldl')
import Data.Text (Text)
import Path (fromAbsFile)
import System.Console.Haskeline (defaultSettings, getHistory, getInputLine)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO
import Control.Parallel.Strategies
-- import Control.DeepSeq

import qualified Data.Text as T
import qualified Text.Megaparsec.Error as ME

import Hibet.Interpretator
import Hibet.Language
import Labels (labels)
import Parse
import Pretty
import Translate (getAnswer, makeTextMap, selectDict, toDictionaryMeta)
import Types


-- | Load environment and start loop dialog
app :: [Int] -> IO ()
app selectedIds = do
    void $ runHibet $ makeEnv selectedIds
    -- env <- runHibet $ makeEnv selectedIds
    -- bracketOnError
    --   (initializeInput defaultSettings)
    --   cancelInput -- This will only be called if an exception such as a SigINT is received.
    --   (\inputState -> runHibet (loopDialog env inputState) >> closeInput inputState)


-- Make environment
makeEnv :: [Int] -> Hibet Env
makeEnv selectedIds = do
    sylsPath <- getDataFileNameH "stuff/tibetan-syllables"
    syls <- getContentH sylsPath
    ls <- labels
    dir <- getDataFileNameH "dicts/"
    (_, files') <- listDirectoryH dir
    filesAndTexts <- traverse getFilesTexts files' -- TODO make lazy loading
    let dictsMeta = parMap rpar (\(f,t) -> toDictionaryMeta ls f $ makeTextMap t) filesAndTexts
    let dmList = selectDict selectedIds dictsMeta
    let wt = makeWylieTibet syls
    let tw = makeTibetWylie syls
    let radixWylie = makeWylieRadexTree syls
    let radixTibet = makeTibetanRadexTree syls
    pure Env
            { envDictionaryMeta = dmList
            , envWylieTibet = wt
            , envTibetWylie = tw
            , envRadixWylie = radixWylie
            , envRadixTibet = radixTibet
            }
  where
    getFilesTexts fp = do
      let path = fromAbsFile fp
      txt <- getContentH path
      pure (path, txt)

-- Looped dialog with user
loopDialog :: Env -> InputState -> Hibet ()
loopDialog env inputState = forever $ do
    putColorTextH blue NewLine "Which a tibetan word to translate?"
    mQuery <- queryInputH inputState $ getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just ":q" -> do
            putColorTextH yellow NewLine "Bye-bye!"
            exitH
        Just ":h" -> do
            history <- fromHistory <$> queryInputH inputState getHistory
            mapM_ (putColorTextH id NewLine) history
        Just query -> do
            let answerE = runExcept $ getAnswer query env
            case answerE of
                Left err -> putColorTextH red NewLine $ T.pack $ ME.errorBundlePretty err
                Right (answer, isEmpty) ->
                    if isEmpty then putColorTextH red NewLine "Nothing found"
                    else pprintH answer


fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines
