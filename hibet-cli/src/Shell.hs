module Shell
    ( HeaderStyle (..)
    , headerDoc
    , mkOutput
    , omHeader
    , pickHeader
    , queryHeader
    , sizedHeader
    , translator
    )
where

import Dictionary (Answer, sortOutput)
import Env (Env (..), readEnv)
import Render.Art (artLines, renderQuery)
import Render.Pretty
    ( Line (NewLine)
    , blue
    , pprint
    , putColorDoc
    , red
    , sized
    , supportsTextSizing
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

-- | How the query is drawn: Braille art when the renderer is built and found a
-- Tibetan font, then OSC 66 in kitty, then the plain query. The art is sized
-- only in text lines, so it follows the console font, not the window or the
-- query.
queryHeader :: Text -> IO (Doc AnsiStyle)
queryHeader query = largeHeader query (pretty query)

-- | The syllable drawn by @hibet om@: the same route as the query header, but
-- the fixed block art stays as the fallback instead of plain text.
omHeader :: Text -> IO (Doc AnsiStyle)
omHeader fallback = largeHeader omSyllable (pretty fallback)

omSyllable :: Text
omSyllable = "ༀ"

-- | How the header is drawn, in the order the levels are tried.
data HeaderStyle
    = -- | Braille art that fits the window.
      ArtHeader [Text]
    | -- | OSC 66 in a kitty that draws it.
      SizedHeader
    | -- | Plain query, or the block art of @hibet om@.
      PlainHeader
    deriving stock (Eq, Show)

-- | The first level that works: drawn art that fits the window, then OSC 66 in
-- a terminal that draws it, then the fallback. The four-cell margin keeps the
-- art off the window edge.
pickHeader :: Int -> Bool -> Maybe [Text] -> HeaderStyle
pickHeader width kitty mArt
    | Just rows <- mArt
    , all ((<= width - 4) . T.length) rows =
        ArtHeader rows
    | kitty = SizedHeader
    | otherwise = PlainHeader

largeHeader :: Text -> Doc AnsiStyle -> IO (Doc AnsiStyle)
largeHeader query fallback = do
    lines' <- artLines
    art <- renderQuery lines' query
    width <- terminalWidth
    kitty <- supportsTextSizing
    pure $ case pickHeader width kitty art of
        ArtHeader rows -> vsep (map pretty rows)
        SizedHeader -> sizedHeader width query
        PlainHeader -> fallback

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
