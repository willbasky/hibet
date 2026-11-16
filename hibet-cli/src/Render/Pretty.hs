module Render.Pretty
    ( -- Colors
      blue
    , cyan
    , green
    , magenta
    , red
    , yellow
    , Colorize
    -- Functions
    , textToColorText
    , viewTranslations
    , withHeader
    , withHeaderSpaces
    -- Output
    , Line (..)
    , kittyTextSizing
    , pageRows
    , paginate
    , parseKittyVersion
    , pprint
    , putColorDoc
    , putColorDoc'
    , putColorList
    , sized
    , supportsTextSizing
    , terminalWidth
    , terminalHeight
    ) where

import Dictionary (Answer (..), Target (..))
import Label (Title (..))
import Render.Keys (Key (..), withRawKeys)
import Utility (showT)

import Control.Exception (IOException, try)
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.List (intersperse, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prettyprinter
    ( Doc
    , LayoutOptions (..)
    , PageWidth (..)
    , annotate
    , defaultLayoutOptions
    , fillSep
    , hang
    , layoutSmart
    , pretty
    , space
    , vsep
    )
import Prettyprinter.Render.Terminal
    ( AnsiStyle
    , Color (..)
    , bold
    , color
    , putDoc
    , renderStrict
    )
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hIsTerminalDevice, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | Header
withHeader :: Colorize -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader col header value =
    hang indentation $ vsep [col $ pretty header, value]

-- | Header with spaces
withHeaderSpaces :: Colorize -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
withHeaderSpaces col header value =
    hang indentation $ vsep [space, col header, space, value]

-- | kitty's text sizing protocol: the terminal draws the text at @scale@ times
-- its cell size, so it takes @scale@ rows and @scale@ times the width. The
-- protocol leaves the cursor on the first of those rows, so move it down to the
-- last one; the following newline then starts below the block. Only call this
-- for a terminal that supports the protocol: others may drop the text.
sized :: Int -> Text -> Text
sized scale text =
    "\ESC]66;s="
        <> showT scale
        <> ";"
        <> text
        <> "\a"
        <> if scale > 1 then "\ESC[" <> showT (scale - 1) <> "B" else ""

-- | Rows a rendered line takes: a line drawn through the sizing protocol at
-- scale N occupies N rows, everything else takes one.
lineRows :: Text -> Int
lineRows line = case T.breakOn oscSizePrefix line of
    (_, rest)
        | not (T.null rest) ->
            case readMaybe . T.unpack . T.takeWhile isDigit $
                T.drop (T.length oscSizePrefix) rest of
                Just scale -> max 1 scale
                Nothing -> 1
    _ -> 1

oscSizePrefix :: Text
oscSizePrefix = "\ESC]66;s="

-- | Terminal width, 80 when the size is unknown.
terminalWidth :: IO Int
terminalWidth = maybe 80 Terminal.width <$> Terminal.size

-- | Terminal height, 24 when the size is unknown.
terminalHeight :: IO Int
terminalHeight = maybe 24 Terminal.height <$> Terminal.size

-- | Whether the terminal draws OSC 66 text: only kitty does, and only from 0.40
-- on, where the text sizing protocol appeared. kitty exports no version
-- variable and the protocol has no reply, so the version is asked from the
-- @kitty@ binary. A missing binary (ssh, a macOS app bundle) leaves the version
-- unknown, and the text is drawn as it was before the check.
supportsTextSizing :: IO Bool
supportsTextSizing = do
    term <- maybe "" id <$> lookupEnv "TERM"
    pure $ "kitty" `isInfixOf` term && maybe True (>= kittyTextSizing) kittyVersion

-- | First kitty version with the text sizing protocol.
kittyTextSizing :: (Int, Int)
kittyTextSizing = (0, 40)

-- | Version of the installed kitty, asked once per process: the shell wants it
-- for every query and it cannot change under a running one.
kittyVersion :: Maybe (Int, Int)
kittyVersion = unsafePerformIO $ do
    result <-
        try (readProcessWithExitCode "kitty" ["--version"] "") ::
            IO (Either IOException (ExitCode, String, String))
    pure $ case result of
        Right (ExitSuccess, out, _) -> parseKittyVersion out
        _ -> Nothing
{-# NOINLINE kittyVersion #-}

-- | Major and minor version from the line @kitty --version@ prints, e.g.
-- @kitty 0.50.0 created by Kovid Goyal@.
parseKittyVersion :: String -> Maybe (Int, Int)
parseKittyVersion output = case words output of
    ("kitty" : version : _) -> case break (== '.') version of
        (major, '.' : minor) ->
            (,) <$> readMaybe major <*> readMaybe (takeWhile isDigit minor)
        _ -> Nothing
    _ -> Nothing

indentation :: Int
indentation = 2

wrapLines :: Text -> Doc ann
wrapLines =
    vsep . map (fillSep . map pretty . T.split isSpace) . T.splitOn "\n"

sparsedStack :: [Doc ann] -> Doc ann
sparsedStack = vsep . intersperse space

---------------------------------------------------------------------
-- Colors
---------------------------------------------------------------------

type Colorize = Doc AnsiStyle -> Doc AnsiStyle

red :: Colorize
red = annotate $ color Red <> bold

green :: Colorize
green = annotate $ color Green <> bold

blue :: Colorize
blue = annotate $ color Blue <> bold

cyan :: Colorize
cyan = annotate $ color Cyan <> bold

magenta :: Colorize
magenta = annotate $ color Magenta <> bold

yellow :: Colorize
yellow = annotate $ color Yellow <> bold

---------------------------------------------------------------------
-- Pretty functions
---------------------------------------------------------------------

-- | Pretty view translation.
viewTranslations :: [Answer] -> Doc AnsiStyle
viewTranslations = sparsedStack . map viewTranslation
    where
        viewTranslation :: Answer -> Doc AnsiStyle
        viewTranslation Answer{..} =
            withHeader green (header dictNumber dictTitle) $ prettyTargets targets
        -- Compose header
        header :: Int -> Title -> Text
        header number title = T.concat [showT number, ". ", unTitle title]
        -- Decode targets
        prettyTargets :: [Target] -> Doc AnsiStyle
        prettyTargets = vsep . map (wrapLines . fixNewLine . unTarget)
        -- Fix new lines inside targets
        fixNewLine :: Text -> Text
        fixNewLine = T.replace "\\n" "\n"

textToColorText :: Colorize -> Text -> Text
textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt

---------------------------------------------------------------------
-- Terminal output
---------------------------------------------------------------------

data Line = NewLine | CurrentLine

putColorDoc :: Colorize -> Line -> Text -> IO ()
putColorDoc col line = putColorDoc' col line . pretty

-- | Like 'putColorDoc', but the document is already built.
putColorDoc' :: Colorize -> Line -> Doc AnsiStyle -> IO ()
putColorDoc' col line doc = putDoc $ col $ case line of
    NewLine -> doc <> pretty ("\n" :: Text)
    CurrentLine -> doc

putColorList :: [(Colorize, Text)] -> IO ()
putColorList = traverse_ (\(c, d) -> putColorDoc c CurrentLine d)

-- | Render the answer. On a terminal the answers are paged on the alternate
-- screen with the query pinned above them; when the pager closes the whole
-- answer is printed once, so the query and the text stay in the scrollback as
-- they do without a pager.
pprint :: Doc AnsiStyle -> Doc AnsiStyle -> IO ()
pprint header doc = do
    isTerminal <- hIsTerminalDevice stdout
    mSize <- Terminal.size
    let width = maybe 80 Terminal.width mSize
        height = maybe 24 Terminal.height mSize
        layoutOptions =
            defaultLayoutOptions{layoutPageWidth = AvailablePerLine width 1}
        render = renderStrict . layoutSmart layoutOptions
        output = render doc
        prefix = hang indentation $ vsep [space, header, space]
        (headerLines, bodyLines) = splitAt (length (T.lines (render prefix))) (T.lines output)
    if isTerminal
        then paged height headerLines bodyLines output
        else TIO.putStr (T.snoc output '\n')

-- | Show the answers a window at a time with the header pinned above. A pager
-- like @less@ cannot be used: it strips OSC 66 (the scaled query) and does not
-- know how much room the scaled line takes. The window lives on the alternate
-- screen, so it leaves no copies behind; on exit the full answer is printed to
-- the normal screen once, and the scrollback holds the query and the text as
-- without a pager. Keys are read raw by "Render.Keys", so arrows and PgUp/PgDn work;
-- @j@/@k@ and Ctrl-N/Ctrl-P move a line, Space/@b@, PgUp/PgDn and
-- Ctrl-F/Ctrl-B or Ctrl-V/Alt-V move a page, @q@ quits.
paged :: Int -> [Text] -> [Text] -> Text -> IO ()
paged height header body output =
    case paginate pageSize body of
        [] -> TIO.putStr (T.snoc output '\n')
        [_] -> TIO.putStr (T.snoc output '\n')
        _ -> do
            TIO.putStr "\ESC[?1049h"
            withRawKeys $ \readKey -> do
                draw 0
                loop readKey 0
            TIO.putStr "\ESC[?1049l"
            TIO.putStr (T.snoc output '\n')
    where
        pageSize = max 1 (height - 1 - pageRows header)
        lastStart = max 0 (length body - pageSize)

        draw offset = do
            TIO.putStr "\ESC[H\ESC[2J"
            traverse_ TIO.putStrLn (header <> take pageSize (drop offset body))
            putColorDoc cyan CurrentLine (keys offset lastStart)
            hFlush stdout

        loop readKey offset = do
            key <- readKey
            case key of
                Quit -> pure ()
                Other -> loop readKey offset
                _ -> do
                    let offset' = move offset key
                    draw offset'
                    loop readKey offset'

        move offset key = case key of
            LineUp -> max 0 (offset - 1)
            LineDown -> min lastStart (offset + 1)
            PageUp -> max 0 (offset - pageSize)
            PageDown -> min lastStart (offset + pageSize)
            _ -> offset

-- | Rows a page takes on screen: a scaled line takes as many rows as its scale.
pageRows :: [Text] -> Int
pageRows = sum . map lineRows

-- | Keys that work from this offset.
keys :: Int -> Int -> Text
keys offset lastStart
    | offset <= 0 = "-- ↓/j: down  PgDn/Space: page down  q: quit --"
    | offset >= lastStart = "-- ↑/k: up  PgUp/b: page up  q: quit --"
    | otherwise = "-- ↑/k: up  ↓/j: down  PgUp/b, PgDn/Space: page  q: quit --"

-- | Split rendered lines into pages of at most @size@ terminal rows. A line
-- drawn at scale N takes N rows, so it lowers the number of lines per page.
paginate :: Int -> [Text] -> [[Text]]
paginate size = go
    where
        n = max 1 size
        go [] = []
        go (line : rest) =
            let (page, left) = fill (lineRows line) [line] rest
             in page : go left
        fill _ page [] = (reverse page, [])
        fill used page (line : rest)
            | used + lineRows line <= n = fill (used + lineRows line) (line : page) rest
            | otherwise = (reverse page, line : rest)
