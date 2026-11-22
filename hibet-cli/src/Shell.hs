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
    , translator
    , viMode
    )
where

import Dictionary (Answer, sortOutput)
import Env (Env (..), readEnv)
import Render.Art (artLines, renderQueryBlocks, warnIfNoFont)
import Render.Pretty
    ( Colorize
    , Line (NewLine)
    , blue
    , pprint
    , putColorDoc
    , putColorDoc'
    , red
    , terminalHeight
    , terminalWidth
    , viewTranslations
    , white
    , withHeaderSpaces
    , yellow
    )
import Render.Wrap (wrapQuery)
import Translator (getAnswer)
import Type (HibetError, renderError)

import Control.Concurrent.MVar (MVar)
import Control.Exception (IOException, bracket, catch)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prettyprinter (Doc, pretty, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified System.Console.Haskeline as Haskeline
import System.Console.Haskeline.History (History, historyLines)
import System.Directory
    ( XdgDirectory (XdgConfig)
    , doesFileExist
    , getHomeDirectory
    , getTemporaryDirectory
    , getXdgDirectory
    , removeFile
    )
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO (hClose, hPutStrLn, openTempFile)

-- | Start the interactive translator over an environment that is loaded in
-- the background.
translator :: Maybe FilePath -> MVar Env -> IO ()
translator dataDir mv = do
    warnIfNoFont dataDir
    putColorDoc
        blue
        NewLine
        "Input a word in the tibetan script or wylie transcription:"
    (prefs, escQuits) <- shellPrefs
    putColorDoc white NewLine (quitHint escQuits)
    Haskeline.runInputTWithPrefs
        prefs
        Haskeline.defaultSettings
        (loopDialog dataDir mv)

quitHint :: Bool -> Text
quitHint escQuits
    | escQuits = "Leave: :q, Ctrl-D, or Esc."
    | otherwise = "Leave: :q or Ctrl-D."

-- | The user's Haskeline preferences plus an Esc binding. Esc is remapped to
-- Ctrl-A, Ctrl-K, Ctrl-D: it clears the line and then becomes EOF. Vi mode
-- uses Esc to leave insert mode, so there the binding is left out.
shellPrefs :: IO (Haskeline.Prefs, Bool)
shellPrefs = do
    mUser <- userPrefsFile
    user <-
        maybe (pure "") TIO.readFile mUser
            `catch` \(_ :: IOException) -> pure ""
    if viMode user
        then do
            prefs <- maybe (pure Haskeline.defaultPrefs) Haskeline.readPrefs mUser
            pure (prefs, False)
        else do
            prefs <- mergedPrefs user
            pure (prefs, True)

-- | Haskeline 0.8.3 reads @$XDG_CONFIG_HOME/haskeline@, 0.8.5 moved it to
-- @haskeline/haskeline@; both fall back to @~/.haskeline@. If both XDG files
-- exist, the newer one wins.
userPrefsFile :: IO (Maybe FilePath)
userPrefsFile = do
    xdgNew <- getXdgDirectory XdgConfig "haskeline/haskeline"
    xdgOld <- getXdgDirectory XdgConfig "haskeline"
    home <- getHomeDirectory
    firstExisting [xdgNew, xdgOld, home </> ".haskeline"]
    where
        firstExisting [] = pure Nothing
        firstExisting (path : paths) = do
            exists <- doesFileExist path
            if exists then pure (Just path) else firstExisting paths

-- | Haskeline's @Prefs@ is abstract, so the binding goes through a temporary
-- prefs file with the user's own settings in front of it.
mergedPrefs :: Text -> IO Haskeline.Prefs
mergedPrefs user = do
    dir <- getTemporaryDirectory
    bracket
        (openTempFile dir "hibet-haskeline.prefs")
        release
        $ \(path, handle) -> do
            hPutStrLn handle (T.unpack user)
            hPutStrLn handle "bind: escape ctrl-a ctrl-k ctrl-d"
            hClose handle
            Haskeline.readPrefs path
    where
        release (path, handle) = do
            hClose handle `catch` ignoreIO
            removeFile path `catch` ignoreIO
        ignoreIO :: IOException -> IO ()
        ignoreIO _ = pure ()

-- | Haskeline's @editmode: Vi@ takes Esc for leaving insert mode. The last
-- @editmode@ line wins, as in Haskeline's own prefs parser.
viMode :: Text -> Bool
viMode = List.foldl' step False . T.lines
    where
        step current line = case T.breakOn ":" line of
            (name, value)
                | T.toLower (T.strip name) == "editmode" ->
                    T.strip (T.drop 1 value) == "Vi"
                | otherwise -> current

-- Looped dialog with user
loopDialog ::
    Maybe FilePath
    -> MVar Env
    -> Haskeline.InputT IO ()
-- Not 'forever': at EOF 'getInputLine' keeps returning 'Nothing', so 'forever'
-- would spin on the closed input. Returning lets 'runInputT' close the session.
loopDialog dataDir mv = do
    mQuery <- Haskeline.getInputLine "> "
    case T.strip . T.pack <$> mQuery of
        Nothing -> pure ()
        Just "" -> pure ()
        Just ":q" -> liftIO $ do
            header <- farewellHeader dataDir
            putColorDoc' id NewLine header
            exitSuccess
        Just ":h" -> do
            history <- fromHistory <$> Haskeline.getHistory
            liftIO $ mapM_ (putColorDoc id NewLine) history
        Just input -> liftIO $ do
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
                        pprint header (mkOutput header answer)
    case mQuery of
        Nothing -> pure ()
        Just _ -> loopDialog dataDir mv

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
-- Tibetan font, then the plain query. The art is sized only in text lines, so
-- it follows the console font, not the window or the query.
queryHeader :: Maybe FilePath -> Text -> IO (Doc AnsiStyle)
queryHeader dataDir query = do
    lines' <- queryArtLines
    largeHeader (const id) dataDir lines' query query

-- | The farewell printed by @:q@: Tibetan only, no English text.
farewell :: Text
farewell = "ཞེས་བསྟན་འཛིན་བཟང་པོ།"

-- | The farewell drawn through the same route and height as a query, in the
-- colors of the Tibetan flag.
farewellHeader :: Maybe FilePath -> IO (Doc AnsiStyle)
farewellHeader dataDir = do
    lines' <- queryArtLines
    largeHeader flagBand dataDir lines' farewell farewell

-- | The syllable drawn by @hibet om@: the same route as the query header, but
-- the fixed block art stays as the fallback instead of plain text.
omHeader :: Maybe FilePath -> Text -> IO (Doc AnsiStyle)
omHeader dataDir fallback = do
    lines' <- omArtLines
    largeHeader flagBand dataDir lines' omSyllable fallback

-- | Colors of the Tibetan flag, one per line: the sun, the red and blue rays,
-- the snow mountain.
flagBand :: Int -> Colorize
flagBand i = case i `mod` 4 of
    0 -> yellow
    1 -> red
    2 -> blue
    _ -> white

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
    | -- | Plain query, or the block art of @hibet om@.
      PlainHeader
    deriving stock (Eq, Show)

-- | The first level that works: drawn art that fits the window, then the
-- fallback. The four-cell margin keeps the art off the window edge; one row is
-- left for the answer and one for the pager keys.
pickHeader :: Int -> Int -> Maybe [Text] -> HeaderStyle
pickHeader width height mArt
    | Just rows <- mArt
    , not (null rows)
    , all ((<= width - 4) . T.length) rows
    , length rows <= height - 4 =
        ArtHeader rows
    | otherwise = PlainHeader

largeHeader ::
    (Int -> Colorize) -> Maybe FilePath -> Int -> Text -> Text -> IO (Doc AnsiStyle)
largeHeader colorAt dataDir lines' query fallback = do
    width <- terminalWidth
    height <- terminalHeight
    blocks <- renderQueryBlocks dataDir width lines' query
    pure $ case pickHeader width height (concat <$> blocks) of
        ArtHeader rows -> vsep (colorRows colorAt rows)
        PlainHeader ->
            vsep $
                colorRows
                    colorAt
                    (concatMap (T.splitOn "\n") (wrapQuery width fallback))

-- | Color the rows, giving each row with ink the next color; blank separators
-- keep the art's spacing and take no band.
colorRows :: (Int -> Colorize) -> [Text] -> [Doc AnsiStyle]
colorRows colorAt = go 0
    where
        go _ [] = []
        go i (row : rows)
            | T.null (T.strip row) = pretty row : go i rows
            | otherwise = colorAt i (pretty row) : go (i + 1) rows
