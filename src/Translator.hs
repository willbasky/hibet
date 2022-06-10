module Translator
  ( translator
  )
  where

import Dictionary (Answer, searchTranslation, sortOutput)
import Effects.Console (Console, cancelInput, closeInput, exitSuccess, getHistory, getInput,
                        initializeInput)
import Effects.PrettyPrint (Line (NewLine), PrettyPrint, pprint, putColorDoc)
import Env (Env)
import Parse (fromTibetScript, fromWylieScript, parseEither, parseTibetanInput, parseWylieInput,
              tibetanWord, toTibetan, toWylie, wylieWord)
import Pretty (blue, red, viewTranslations, withHeaderSpaces, yellow)
import Type (HibetError (..))

import Control.Monad.Except (Except, forever, liftEither, runExcept)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Reader (Reader, ask)
import Polysemy.Resource (Resource, bracketOnError)
import Polysemy.Trace (Trace, trace)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)

import qualified Debug.Trace as Debug

-- | Load environment and start loop dialog
translator :: Members [PrettyPrint, Trace, Resource, Console, Error HibetError, Reader Env ] r
  => Sem r ()
translator = bracketOnError
  initializeInput
  cancelInput -- This will only be called if an exception such as a SigINT is received.
  $ \inputState -> do
      putColorDoc blue NewLine "Please, input a word with tibetan script or wylie transcription!"
      loopDialog inputState
      closeInput inputState


-- Looped dialog with user
loopDialog :: Members
  [ PrettyPrint
  , Trace
  , Console
  , Error HibetError
  , Reader Env ] r
  => InputState
  -> Sem r ()
loopDialog inputState = forever $ do
    putColorDoc blue NewLine "Your request:"
    mQuery <- getInput inputState "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "Bye-bye!"
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> getHistory inputState
            mapM_ (putColorDoc id NewLine) history
        Just input -> do
            env :: Env <- ask
            case runExcept $ getAnswer input env of
              Left err -> do
                trace $ show err
                putColorDoc red NewLine "Nothing found"
              Right (query, answer) -> if null answer
                then putColorDoc red NewLine "Nothing found"
                else pprint $ mkOutput query answer

fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines

data Script = T | W
  deriving stock Show

getAnswer :: Text -> Env -> Except HibetError (Text, [Answer])
getAnswer query env = do
  -- 1. Detect script of input
  -- Debug.traceM ("query " <> T.unpack query)
  let eWylie = parseEither wylieWord query
  let eTibetan = parseEither tibetanWord query
  script <- liftEither $ case (eWylie, eTibetan) of
        (Left _, Right _)  -> Right T -- likely tibetan
        (Right _, Right _) -> Right W -- likely wylie
        (Left ew, Left _)  -> Left ew -- What is it?
        (Right _, Left et) -> Left et -- What is it?
  Debug.traceM $ show script

  -- 2. Prepare wylie and tibetan scripts
  (queryWylie, queryTibet) <- liftEither $ case script of
    W -> do
      wList <- parseWylieInput env.radixWylie query
      wylieText <- fromWylieScript wList
      tList <- toTibetan env.wylieTibetMap wList
      tibetanText <- fromTibetScript tList
      Right (wylieText, tibetanText)
    T -> do
      -- 2.1. Parse text to tibetan script,
      -- 2.2. check tibetan script is valid,
      -- 2.3. convert to Wylie.
      tList <- parseTibetanInput env.radixTibet query
      tibetanText <- fromTibetScript tList
      -- Debug.traceM ("tList " <> show tList)
      wList <- toWylie env.tibetWylieMap tList
      -- Debug.traceM ("wList " <> show wList)
      wylieText <- fromWylieScript wList
      -- Debug.traceM ("wylieText " <> show wylieText)
      Right (wylieText, tibetanText)
  -- 3. Translate
  let dscValues = mapMaybe (searchTranslation queryWylie) env.dictionaryMeta `using` parList rseq
  -- 5. Return
  pure (queryTibet, dscValues)

mkOutput :: Text -> [Answer] -> Doc AnsiStyle
mkOutput query answers
  = withHeaderSpaces yellow query
  $ viewTranslations
  $ sortOutput answers
