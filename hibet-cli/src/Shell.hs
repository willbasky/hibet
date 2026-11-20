module Shell
    ( HeaderStyle (..)
    , farewell
    , farewellHeader
    , mkOutput
    , omArtLines
    , omHeader
    , pickHeader
    , queryArtLines
    , queryHeader
    , queryReply
    , sizedHeader
    , translator
    )
where

import Dictionary (Answer, sortOutput)
import Env (Env (..), readEnv)
import Render.Art (artLines, renderQueryBlocks, warnIfNoFont)
import Render.Pretty
    ( Line (NewLine)
    , blue
    , pprint
    , putColorDoc
    , putColorDoc'
    , red
    , sized
    , supportsTextSizing
    , terminalHeight
    , terminalWidth
    , viewTranslations
    , withHeaderSpaces
    , yellow
    )
import Render.Wrap (wrapQuery)
import Translator (getAnswer)
import Type (HibetError, renderError)

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
import System.IO (hIsTerminalDevice, stdout)

-- | Start the interactive translator over an environment that is loaded in
-- the background.
translator :: Maybe FilePath -> MVar Env -> IO ()
translator dataDir mv = bracketOnError
    (Haskeline.initializeInput Haskeline.defaultSettings)
    Haskeline.cancelInput -- This will only be called if an exception such as a SigINT is received.
    $ \inputState -> do
        warnIfNoFont dataDir
        putColorDoc
            blue
            NewLine
            "Input a word in the tibetan script or wylie transcription:"
        loopDialog dataDir mv inputState
        Haskeline.closeInput inputState

-- Looped dialog with user
loopDialog ::
    Maybe FilePath
    -> MVar Env
    -> InputState
    -> IO ()
-- Not 'forever': at EOF 'getInput' keeps returning 'Nothing', so 'forever'
-- would spin on the closed input. Returning lets 'translator' call 'closeInput'.
loopDialog dataDir mv inputState = do
    mQuery <- Haskeline.queryInput inputState $ Haskeline.getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just "" -> pure ()
        Just ":q" -> do
            header <- farewellHeader dataDir
            putColorDoc' yellow NewLine header
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
                else case queryReply $ getAnswer input env of
                    Left message -> do
                        putColorDoc red NewLine message
                    Right (query, answer) -> do
                        header <- queryHeader dataDir query
                        pprint
                            header
                            (mkOutput header answer)
    case mQuery of
        Nothing -> pure ()
        Just _ -> loopDialog dataDir mv inputState

-- | What the shell shows for a query: the reason of a rejected query, the
-- «Nothing found» marker of an empty answer, or the answer to draw.
queryReply ::
    Either HibetError (Text, [Answer])
    -> Either Text (Text, [Answer])
queryReply (Left err) = Left (renderError err)
queryReply (Right (_, [])) = Left "Nothing found"
queryReply (Right reply) = Right reply

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
queryHeader :: Maybe FilePath -> Text -> IO (Doc AnsiStyle)
queryHeader dataDir query = do
    lines' <- queryArtLines
    largeHeader dataDir lines' query query

-- | The farewell printed by @:q@: Tibetan only, no English text.
farewell :: Text
farewell = "ཞེས་བསྟན་འཛིན་བཟང་པོ།"

-- | The farewell drawn through the same route and height as a query.
farewellHeader :: Maybe FilePath -> IO (Doc AnsiStyle)
farewellHeader dataDir = queryHeader dataDir farewell

-- | The syllable drawn by @hibet om@: the same route as the query header, but
-- the fixed block art stays as the fallback instead of plain text.
omHeader :: Maybe FilePath -> Text -> IO (Doc AnsiStyle)
omHeader dataDir fallback = do
    lines' <- omArtLines
    largeHeader dataDir lines' omSyllable fallback

omSyllable :: Text
omSyllable = "ༀ"

-- | Height of the query art in text lines. The code default; the
-- @HIBET_ART_LINES@ environment variable only overrides it.
defaultQueryLines :: Int
defaultQueryLines = 4

-- | Height of the @hibet om@ art in text lines. The code default; the
-- @HIBET_OM_LINES@ environment variable only overrides it.
defaultOmLines :: Int
defaultOmLines = 10

-- | Query art height: @HIBET_ART_LINES@ when it holds a positive number,
-- otherwise 'defaultQueryLines'.
queryArtLines :: IO Int
queryArtLines = artLines "HIBET_ART_LINES" defaultQueryLines

-- | @hibet om@ art height: @HIBET_OM_LINES@ when it holds a positive number,
-- otherwise 'defaultOmLines'.
omArtLines :: IO Int
omArtLines = artLines "HIBET_OM_LINES" defaultOmLines

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
-- art off the window edge; one row is left for the answer and one for the
-- pager keys.
pickHeader :: Int -> Int -> Bool -> Maybe [Text] -> HeaderStyle
pickHeader width height kitty mArt
    | Just rows <- mArt
    , not (null rows)
    , all ((<= width - 4) . T.length) rows
    , length rows <= height - 4 =
        ArtHeader rows
    | kitty = SizedHeader
    | otherwise = PlainHeader

largeHeader :: Maybe FilePath -> Int -> Text -> Text -> IO (Doc AnsiStyle)
largeHeader dataDir lines' query fallback = do
    isTerminal <- hIsTerminalDevice stdout
    width <- terminalWidth
    height <- terminalHeight
    blocks <- renderQueryBlocks dataDir width lines' query
    kitty <- supportsTextSizing
    -- A redirected stdout would keep the OSC 66 escape bytes around the query.
    pure $ case pickHeader width height (isTerminal && kitty) (concat <$> blocks) of
        ArtHeader rows -> vsep (map pretty rows)
        SizedHeader -> sizedHeader width query
        PlainHeader -> vsep (map pretty (wrapQuery width fallback))

-- | The query drawn through OSC 66 at the largest scale that fits @width@.
-- The scale is chosen once for the widest wrapped line, so the lines do not
-- jump in size.
sizedHeader :: Int -> Text -> Doc AnsiStyle
sizedHeader width query
    | scale > 1 = vsep (map (pretty . sized scale) rows)
    | otherwise = vsep (map pretty rows)
    where
        rows = wrapQuery width query
        widest = maximum (0 : map T.length rows)
        scale = max 1 (min 4 ((width - 4) `div` max 1 widest))
