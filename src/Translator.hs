{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Translator
  ( translator
  )
  where

import Effects.Console
import Effects.PrettyPrint
import Pretty
import Parse
import Env (Env)
import Dictionary

import Control.Monad.Except
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Members, Sem)
import Polysemy.Resource (Resource, bracketOnError)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)
import qualified Text.Megaparsec.Error as ME


-- | Load environment and start loop dialog
translator :: Members [PrettyPrint, Resource, Console] r
  => Env
  -> Sem r ()
translator env = bracketOnError
  initializeInput
  cancelInput -- This will only be called if an exception such as a SigINT is received.
  (\inputState -> loopDialog env inputState >> closeInput inputState)
  -- (\inputState -> runReaderT (testDialog inputState) env >> closeInput inputState) -- for tests


-- Looped dialog with user
loopDialog :: Members [PrettyPrint, Console] r
  => Env
  -> InputState
  -> Sem r ()
loopDialog env inputState = forever $ do
    putColorDoc blue NewLine "Input a tibetan word in wylie transcription, please."
    mQuery <- getInput inputState "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "Bye-bye!"
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> getHistory inputState
            mapM_ (putColorDoc id NewLine) history
        Just query -> do
            let answerE = runExcept $ getAnswer query env
            case answerE of
                Left err -> putColorDoc red NewLine $ T.pack $ ME.errorBundlePretty err
                Right (answer, isEmpty) ->
                    if isEmpty then putColorDoc red NewLine "Nothing found"
                    else pprint answer

fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines


getAnswer :: Text -> Env -> Except ParseError (Doc AnsiStyle, Bool)
getAnswer query env = do
  let toWylie' = toWylie env.tibetWylie . parseTibetanInput env.radixTibet
      queryWylie = case runExcept $ toWylie' query  of
        Left _      -> query
        Right wylie -> if T.null wylie then query else wylie
      dscValues = mapMaybe (searchTranslation queryWylie) env.dictionaryMeta `using` parList rseq
  let list = sortOutput dscValues
  let toTibetan' = toTibetan env.wylieTibet . parseWylieInput env.radixWylie
  -- list <- traverse (separator [37] toTibetan') dictMeta
  let (translations, isEmpty) = (viewTranslations list, list == mempty)
  query' <- if query == queryWylie
    then T.concat <$> toTibetan' queryWylie
    else pure queryWylie
  pure (withHeaderSpaces yellow query' translations, isEmpty)


-- testDialog :: InputState -> Hibet ()
-- testDialog inputState = ReaderT $ \env -> forever $ do
--   putColorDoc blue NewLine "Which a tibetan word to translate?"
--   let answerE = runExcept $ getAnswer "mo" env
--   case answerE of
--       Left err -> putColorDoc red NewLine $ T.pack $ ME.errorBundlePretty err
--       Right (answer, isEmpty) ->
--           if isEmpty then putColorDoc red NewLine "Nothing found"
--           else print answer
--   exitSuccess
