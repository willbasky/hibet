module Shell
    ( headerDoc
    , mkOutput
    , queryHeader
    , sizedHeader
    , translator
    )
where

import Dictionary (Answer, sortOutput)
import Env (Env (..), readEnv)
import Render.Art (renderQuery)
import Render.Pretty
    ( Line (NewLine)
    , blue
    , pprint
    , putColorDoc
    , red
    , sized
    , supportsTextSizing
    , terminalHeight
    , terminalWidth
    , viewTranslations
    , withHeaderSpaces
    , yellow
    )
import Translator (getAnswer)

import Control.Concurrent.MVar (MVar)
import Control.Exception (bracketOnError)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc, pretty, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.Console.Haskeline as Haskeline
import System.Console.Haskeline.History (History, historyLines)
import System.Console.Haskeline.IO (InputState)
import qualified System.Console.Haskeline.IO as Haskeline
import System.Exit (exitSuccess)

-- | Start the interactive translator over an environment that is loaded in
-- the background.
translator :: MVar Env -> IO ()
translator mv = bracketOnError
    (Haskeline.initializeInput Haskeline.defaultSettings)
    Haskeline.cancelInput -- This will only be called if an exception such as a SigINT is received.
    $ \inputState -> do
        putColorDoc
            blue
            NewLine
            "Input a word in the tibetan script or wylie transcription:"
        loopDialog mv inputState
        Haskeline.closeInput inputState

-- Looped dialog with user
loopDialog ::
    MVar Env
    -> InputState
    -> IO ()
-- Not 'forever': at EOF 'getInput' keeps returning 'Nothing', so 'forever'
-- would spin on the closed input. Returning lets 'translator' call 'closeInput'.
loopDialog mv inputState = do
    mQuery <- Haskeline.queryInput inputState $ Haskeline.getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just "" -> pure ()
        Just ":q" -> do
            putColorDoc yellow NewLine "ཞེས་བསྟན་འཛིན་བཟང་པོ། Bye!"
            exitSuccess
        Just ":h" -> do
            history <-
                fromHistory
                    <$> Haskeline.queryInput inputState Haskeline.getHistory
            mapM_ (putColorDoc id NewLine) history
        Just input -> do
            env <- readEnv mv
            if null env.dictionaryMeta
                then
                    putColorDoc
                        red
                        NewLine
                        "No dictionaries are selected. Run 'hibet show names' to list ids."
                else case getAnswer input env of
                    Left _ -> do
                        putColorDoc red NewLine "Nothing found"
                    Right (query, answer) ->
                        if null answer
                            then putColorDoc red NewLine "Nothing found"
                            else do
                                header <- queryHeader query
                                pprint
                                    header
                                    (mkOutput header answer)
    case mQuery of
        Nothing -> pure ()
        Just _ -> loopDialog mv inputState

fromHistory :: History -> [Text]
fromHistory =
    reverse
        . map T.pack
        . filter (/= ":h")
        . historyLines

mkOutput :: Doc AnsiStyle -> [Answer] -> Doc AnsiStyle
mkOutput header answers =
    withHeaderSpaces yellow header
        $ viewTranslations
        $ sortOutput answers

-- | How the header is drawn: kitty scales the text through OSC 66. Other
-- terminals get Braille art when the renderer is built and a Tibetan font is
-- found; otherwise the plain query, because a terminal that does not know
-- OSC 66 may drop the text with it.
queryHeader :: Text -> IO (Doc AnsiStyle)
queryHeader query = do
    kitty <- supportsTextSizing
    width <- terminalWidth
    if kitty
        then pure (sizedHeader width query)
        else do
            height <- terminalHeight
            headerDoc query <$> renderQuery width height query

-- | The query above the answers: Braille art when the renderer drew it,
-- plain text otherwise.
headerDoc :: Text -> Maybe [Text] -> Doc AnsiStyle
headerDoc query = maybe (pretty query) (vsep . map pretty)

-- | The query drawn through OSC 66 at the largest scale that fits @width@.
sizedHeader :: Int -> Text -> Doc AnsiStyle
sizedHeader width query
    | scale > 1 = pretty (sized scale query)
    | otherwise = pretty query
    where
        scale = max 1 (min 4 ((width - 4) `div` max 1 (T.length query)))
