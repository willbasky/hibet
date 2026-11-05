module Translator
    ( translator
    , getAnswer
    )
where

import Convert
    ( OutputFormat (OutWylie)
    , SpellItem (SyllableItem)
    , convertWylieToUnicode
    , defaultSettings
    , renderItems
    , splitSentences
    )
import Dictionary (Answer, searchTranslation, sortOutput)
import Effects.Console (Console, closeInput, exitSuccess, getHistory, getInput)
import Effects.PrettyPrint (Line (NewLine), PrettyPrint, pprint, putColorDoc)
import Env (Env (..), readEnv)
import Pretty (blue, red, viewTranslations, withHeaderSpaces, yellow)
import Type (HibetError (..))

import Control.Monad (forever)
import Control.Parallel.Strategies (parList, rseq, using)
import Data.Either.Extra (mapLeft)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.Console.Haskeline as Haskeline
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)
import qualified System.Console.Haskeline.IO as Haskeline

import Effectful (Eff, IOE, type (:>))
import Effectful.Resource (Resource, allocate, release)

-- import Effectful.Log ( logInfo_, Log )

import Effectful.Concurrent.MVar (Concurrent, MVar)
import Effectful.Reader.Dynamic (Reader)

-- | Load environment and start loop dialog
translator ::
    ( IOE :> es
    , PrettyPrint :> es
    , -- , Log :> es
      Resource :> es
    , Console :> es
    , Reader (MVar Env) :> es
    , Concurrent :> es
    ) =>
    Eff es ()
translator = bracketOnError
    (Haskeline.initializeInput Haskeline.defaultSettings)
    Haskeline.cancelInput -- This will only be called if an exception such as a SigINT is received.
    $ \inputState -> do
        putColorDoc
            blue
            NewLine
            "Input a word in the tibetan script or wylie transcription:"
        loopDialog inputState
        closeInput inputState

-- Looped dialog with user
loopDialog ::
    ( PrettyPrint :> es
    , Console :> es
    , Reader (MVar Env) :> es
    , Concurrent :> es
    ) =>
    InputState
    -> Eff es ()
loopDialog inputState = forever $ do
    mQuery <- getInput inputState "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just "" -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "ཞེས་བསྟན་འཛིན་བཟང་པོ། Bye!"
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> getHistory inputState
            mapM_ (putColorDoc id NewLine) history
        Just input -> do
            env <- readEnv
            case getAnswer input env of
                Left _ -> do
                    -- logInfo_ $ showT err
                    putColorDoc red NewLine "Nothing found"
                Right (query, answer) ->
                    if null answer
                        then putColorDoc red NewLine "Nothing found"
                        else pprint $ mkOutput query answer

getAnswer :: Text -> Env -> Either HibetError (Text, [Answer])
getAnswer query env = do
    -- 1. Detect script of input
    let script = detectScript query
    -- 2. Prepare wylie and tibetan scripts
    (queryWylie, queryTibet) <- boilQuery script query
    -- 3. Translate
    let dscValues =
            mapMaybe (searchTranslation queryWylie) env.dictionaryMeta `using` parList rseq
    -- 5. Return
    pure (queryTibet, dscValues)

fromHistory :: History -> [Text]
fromHistory =
    foldl' (\a x -> T.pack x : a) []
        . filter (/= ":h")
        . historyLines

-- | Which writing the query is in. `hewts` has no auto-detection: a Latin
-- letter may be foreign text inside Tibetan, so the caller says the writing.
-- The Tibetan block is enough here: EWTS input has no characters in it.
data ScriptType = Tibet | Wylie

detectScript :: Text -> ScriptType
detectScript query
    | T.any isTibetan query = Tibet
    | otherwise = Wylie
    where
        isTibetan c = c >= '\x0f00' && c <= '\x0fff'

boilQuery ::
    ScriptType
    -> Text
    -> Either HibetError (Text, Text)
boilQuery script query = case script of
    Wylie -> do
        tibetanText <- fromHewts $ fst <$> convertWylieToUnicode defaultSettings query
        pure (T.unwords (T.words query), tibetanText)
    Tibet -> do
        -- The dictionary is keyed by Wylie words, so punctuation and spaces of the
        -- query must not reach the key: take the syllables through the token layer.
        items <- mapLeft UnknownError $ splitSentences query
        let wylieText =
                T.unwords
                    [renderItems OutWylie [item] | item@SyllableItem{} <- items]
        pure (wylieText, T.strip query)

fromHewts :: Either Text a -> Either HibetError a
fromHewts = mapLeft UnknownError

mkOutput :: Text -> [Answer] -> Doc AnsiStyle
mkOutput query answers =
    withHeaderSpaces yellow query
        $ viewTranslations
        $ sortOutput answers

bracketOnError ::
    (IOE :> es, Resource :> es) =>
    IO a
    -> (a -> IO ())
    -> (a -> Eff es ())
    -> Eff es ()
bracketOnError alloc free inside = do
    (releaseKey, resource) <- allocate alloc free
    inside resource
    release releaseKey
