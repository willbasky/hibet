module Translator
  ( translator
  )
  where

import Dictionary (Answer, searchTranslation, sortOutput)
import Effects.Console (Console, cancelInput, closeInput, exitSuccess, getHistory, getInput,
                        initializeInput)
import Effects.PrettyPrint (Line (NewLine), PrettyPrint, pprint, putColorDoc)
import Env (EnvC, Env)
import Parse (fromTibetScript, fromWylieScript, parseEither, parseTibetanInput, parseWylieInput,
              tibetanWord, toTibetan, toWylie, wylieWord)
import Pretty (blue, red, viewTranslations, withHeaderSpaces, yellow)
import Type (HibetError (..))
import Utility (showT)

import Control.Monad.Except (Except, forever, liftEither, runExcept)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Compact as C
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
translator :: Members [PrettyPrint, Trace, Resource, Console, Error HibetError, Reader EnvC ] r
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
  , Reader EnvC ] r
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
            env :: Env <- C.getCompact <$> ask
            case runExcept $ getAnswer input env of
              Left err -> do
                trace $ show err
                putColorDoc red NewLine "Nothing found"
              Right (query, answer) -> if null answer
                then putColorDoc red NewLine "Nothing found"
                else pprint $ mkOutput query answer


getAnswer :: Text -> Env -> Except HibetError (Text, [Answer])
getAnswer query env = do
  -- 1. Detect script of input
  script <- detectScript query
  -- Debug.traceM $ show script
  -- 2. Prepare wylie and tibetan scripts
  (queryWylie, queryTibet) <- boilQuery script query env
  -- Debug.traceM ("queryTibet " <> T.unpack queryTibet)
  -- 3. Translate
  let dscValues = mapMaybe (searchTranslation queryWylie) env.dictionaryMeta `using` parList rseq
  -- 5. Return
  pure (queryTibet, dscValues)


fromHistory :: History -> [Text]
fromHistory = foldl' (\ a x -> T.pack x : a) [] . filter (/=":h") . historyLines

data Script = T | W
  deriving stock Show

detectScript :: Text -> Except HibetError Script
detectScript query = do
  let eWylie = parseEither wylieWord query
  let eTibetan = parseEither tibetanWord query
  liftEither $ case (eWylie, eTibetan) of
    (Left _, Right _)  -> Right T -- likely tibetan
    (Right _, _) -> Right W -- likely wylie
    (Left ew, Left et)  -> do
      Debug.traceM ("Left ew: " <> show ew)
      Debug.traceM ("Left et: " <> show et)
      Left $ UnknownError (showT et <> "\n" <> showT ew)

boilQuery :: Script -> Text -> Env -> Except HibetError (Text, Text)
boilQuery script query env = liftEither $ case script of
  W -> do
    wList <- parseWylieInput env.radixWylie query
    -- Debug.traceM ("wList " <> show wList)
    wylieText <- fromWylieScript wList
    -- Debug.traceM ("wylieText " <> show wylieText)
    tList <- toTibetan env.wylieTibetMap wList
    -- Debug.traceM ("tList " <> show tList)
    tibetanText <- fromTibetScript tList
    -- Debug.traceM ("tibetanText " <> show tibetanText)
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

mkOutput :: Text -> [Answer] -> Doc AnsiStyle
mkOutput query answers
  = withHeaderSpaces yellow query
  $ viewTranslations
  $ sortOutput answers
