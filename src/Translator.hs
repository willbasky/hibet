module Translator
  ( translator
  )
  where

import Dictionary (searchTranslation, sortOutput)
import Effects.Console (Console, cancelInput, closeInput, exitSuccess, getHistory, getInput,
                        initializeInput)
import Effects.PrettyPrint (Line (NewLine), PrettyPrint, pprint, putColorDoc)
import Env (Env)
import Parse (fromTibetScript, fromWylieScript, parseTibetanInput, parseWylieInput, toTibetan, parseEither,
              toWylie, tibetanWord, wylieWord)
import Pretty (blue, red, viewTranslations, withHeaderSpaces, yellow)
import Type (HibetError (..))

import Control.Monad.Except (Except, forever, runExcept, liftEither)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Members, Sem)
import Polysemy.Error (Error, fromEither)
import Polysemy.Reader (Reader, ask)
import Polysemy.Resource (Resource, bracketOnError)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)

import Polysemy.Trace (Trace)
import qualified Debug.Trace as Debug
-- import Polysemy.Trace (trace)

-- | Load environment and start loop dialog
translator :: Members [PrettyPrint, Trace, Resource, Console, Error HibetError, Reader Env ] r
  => Sem r ()
translator = bracketOnError
  initializeInput
  cancelInput -- This will only be called if an exception such as a SigINT is received.
  (\inputState -> loopDialog inputState >> closeInput inputState)
  -- (\inputState -> runReaderT (testDialog inputState) env >> closeInput inputState) -- for tests


-- Looped dialog with user
loopDialog :: Members [PrettyPrint, Trace, Console, Error HibetError, Reader Env ] r
  => InputState
  -> Sem r ()
loopDialog inputState = forever $ do
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
            env :: Env <- ask
            (answer, isEmpty) <- fromEither $ runExcept $ getAnswer query env
            if isEmpty
              then putColorDoc red NewLine "Nothing found"
              else pprint answer

fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines

data Script = T | W
  deriving stock Show

getAnswer :: Text -> Env -> Except HibetError (Doc AnsiStyle, Bool)
getAnswer query env = do
  -- Debug.traceM ("query " <> T.unpack query)
  let eWylie = parseEither wylieWord query
  let eTibetan = parseEither tibetanWord query
  script <- liftEither $ case (eWylie, eTibetan) of
        (Left _, Right _) -> Right T -- likely tibetan
        (Right _, Right _) -> Right W -- likely wylie
        _ -> Left $ NotParsed query -- What is it?
  Debug.traceM $ show script
  -- TODO: fix partalal case "queryWylie re ba མེདརེ ba med pa pa"
  let queryWylie = case script of
        W -> query
        T -> do
          -- 1. Parse text to tibetan script,
          -- 2. check tibetan script is valid,
          -- 3. convert to Wylie.
          let parsedT = parseTibetanInput env.radixTibet query
          let convertedToWylie = Debug.trace ("parsedT " <> show parsedT) $
                toWylie env.tibetWylieMap parsedT
          case Debug.trace ("convertedToWylie " <> show convertedToWylie) $ runExcept convertedToWylie  of
            Left _      -> query
            Right wylie -> Debug.trace ("wylie" <> show wylie) $ if null wylie then query else T.intercalate " " $ map fromWylieScript wylie
  let dscValues = mapMaybe (searchTranslation queryWylie) env.dictionaryMeta `using` parList rseq

  Debug.traceM ("queryWylie " <> T.unpack queryWylie)
  let list = sortOutput dscValues -- TODO: use Set
  let toTibetan' = toTibetan env.wylieTibetMap . parseWylieInput env.radixWylie
  -- list <- traverse (separator [37] toTibetan') dictMeta
  let (translations, isEmpty) = (viewTranslations list, list == mempty)
  query' <- if query == queryWylie
    then do
      tibetScript <- toTibetan' queryWylie
      pure $ T.intercalate "་" $ fromTibetScript <$> tibetScript
    else pure query
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
