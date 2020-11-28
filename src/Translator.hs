module Translator where

import Pretty
import Dictionary (getAnswer)
import Types

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (foldl')
import Data.Text (Text)
import System.Console.Haskeline (getHistory, getInputLine)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO
import System.Exit (exitSuccess)

import qualified Data.Text as T
import qualified Text.Megaparsec.Error as ME


-- Looped dialog with user
loopDialog :: InputState -> Hibet ()
loopDialog inputState = ReaderT $ \env -> forever $ do
    putColorDoc blue NewLine "Which a tibetan word to translate?"
    mQuery <- queryInput inputState $ getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "Bye-bye!"
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> queryInput inputState getHistory
            mapM_ (putColorDoc id NewLine) history
        Just query -> do
            let answerE = runExcept $ getAnswer query env
            case answerE of
                Left err -> putColorDoc red NewLine $ T.pack $ ME.errorBundlePretty err
                Right (answer, isEmpty) ->
                    if isEmpty then putColorDoc red NewLine "Nothing found"
                    else pprint answer

testDialog :: InputState -> Hibet ()
testDialog inputState = ReaderT $ \env -> forever $ do
  putColorDoc blue NewLine "Which a tibetan word to translate?"
  let answerE = runExcept $ getAnswer "mo" env
  case answerE of
      Left err -> putColorDoc red NewLine $ T.pack $ ME.errorBundlePretty err
      Right (answer, isEmpty) ->
          if isEmpty then putColorDoc red NewLine "Nothing found"
          else print answer
  exitSuccess

fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines
