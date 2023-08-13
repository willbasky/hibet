

module Translator
  ( translator
  )
  where

import Dictionary (Answer, searchTranslation, sortOutput)
import Effects.Console (Console, closeInput, exitSuccess, getHistory, getInput)
import Effects.PrettyPrint (Line (NewLine), PrettyPrint, pprint, putColorDoc)
import Env (Env(..), readEnv)
import Parse (ScriptType (Tibet, Wylie), fromScripts, parseEither, parseTibetanInput,
              parseWylieInput, tibetanWord, toTibetan, toWylie, wylieWord)
import Pretty (blue, red, viewTranslations, withHeaderSpaces, yellow)
import Type (HibetError (..))
import Utility (showT)

import Control.Monad (forever)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)
import qualified System.Console.Haskeline.IO as Haskeline
import qualified System.Console.Haskeline as Haskeline

import Effectful ( type (:>), Eff, IOE )
import Effectful.Resource ( allocate, release, Resource )
import Effectful.Log ( logInfo_, Log )
import Effectful.Reader.Dynamic (Reader)
import Effectful.Concurrent.MVar.Strict (MVar, Concurrent)


-- | Load environment and start loop dialog
translator ::
  ( IOE :> es
  , PrettyPrint :> es
  , Log :> es
  , Resource :> es
  , Console :> es
  , Reader (MVar Env) :> es
  , Concurrent :> es
  )
  => Eff es ()
translator = bracketOnError
  (Haskeline.initializeInput Haskeline.defaultSettings)
  Haskeline.cancelInput -- This will only be called if an exception such as a SigINT is received.
  $ \inputState -> do
      putColorDoc blue NewLine "Input a word in the tibetan script or wylie transcription:"
      loopDialog inputState
      closeInput inputState


-- Looped dialog with user
loopDialog ::
  ( PrettyPrint :> es
  , Log :> es
  , Console :> es
  , Reader (MVar Env) :> es
  , Concurrent :> es
  )
  => InputState
  -> Eff es ()
loopDialog inputState = forever $ do
    mQuery <- getInput inputState "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "ཞེས་བསྟན་འཛིན་བཟང་པོ། Bye!"
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> getHistory inputState
            mapM_ (putColorDoc id NewLine) history
        Just input -> do
            env <- readEnv
            case getAnswer input env of
              Left err -> do
                logInfo_ $ showT err
                putColorDoc red NewLine "Nothing found"
              Right (query, answer) -> if null answer
                then putColorDoc red NewLine "Nothing found"
                else pprint $ mkOutput query answer

getAnswer :: Text -> Env -> Either HibetError (Text, [Answer])
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
fromHistory
  = foldl' (\ a x -> T.pack x : a) []
  . filter (/=":h") . historyLines


detectScript :: Text -> Either HibetError ScriptType
detectScript query = do
  let eWylie = parseEither wylieWord query
  let eTibetan = parseEither tibetanWord query
  case (eWylie, eTibetan) of
    (Left _, Right _)  -> Right Tibet -- likely tibetan
    (Right _, _) -> Right Wylie -- likely wylie
    (Left ew, Left et)  -> do
      Debug.traceM ("Left ew: " <> show ew)
      Debug.traceM ("Left et: " <> show et)
      Left $ UnknownError (showT et <> "\n" <> showT ew)

boilQuery :: ScriptType
  -> Text
  -> Env
  -> Either HibetError (Text, Text)
boilQuery script query env = case script of
  Wylie -> do
    wList <- parseWylieInput env.radixWylie query
    -- Debug.traceM ("wList " <> show wList)
    let wylieText = fromScripts wList
    -- Debug.traceM ("wylieText " <> show wylieText)
    tList <- toTibetan env.wylieTibetMap wList
    -- Debug.traceM ("tList " <> show tList)
    let tibetanText = fromScripts tList
    -- Debug.traceM ("tibetanText " <> show tibetanText)
    Right (wylieText, tibetanText)
  Tibet -> do
    -- 2.1. Parse text to tibetan script,
    -- 2.2. check tibetan script is valid,
    -- 2.3. convert to Wylie.
    tList <- parseTibetanInput env.radixTibet query
    let tibetanText = fromScripts tList
    -- Debug.traceM ("tList " <> show tList)
    wList <- toWylie env.tibetWylieMap tList
    -- Debug.traceM ("wList " <> show wList)
    let wylieText = fromScripts wList
    -- Debug.traceM ("wylieText " <> show wylieText)
    Right (wylieText, tibetanText)

mkOutput :: Text -> [Answer] -> Doc AnsiStyle
mkOutput query answers
  = withHeaderSpaces yellow query
  $ viewTranslations
  $ sortOutput answers

bracketOnError :: (IOE :> es, Resource :> es)
  => IO a
  -> (a -> IO ())
  -> (a -> Eff es ()) -> Eff es ()
bracketOnError alloc free inside = do
  (releaseKey, resource) <- allocate alloc free
  inside resource
  release releaseKey
