module Pretty
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
    , paginate
    , pprint
    , putColorDoc
    , putColorList
    , sized
    , supportsTextSizing
    , terminalWidth
    ) where

import Dictionary (Answer (..), Target (..))
import Label (Title (..))
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
import System.IO (hFlush, hIsTerminalDevice, stdout)
import Text.Read (readMaybe)

-- | Header
withHeader :: Colorize -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader col header value =
    hang indentation $ vsep [col $ pretty header, value]

-- | Header with spaces
withHeaderSpaces :: Colorize -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeaderSpaces col header value =
    hang indentation $ vsep [space, col $ pretty header, space, value]

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

-- | Whether the terminal draws OSC 66 text. Only kitty does among the common
-- terminals; others either ignore the sequence (and lose the query with it) or
-- strip it. A CPR probe from the protocol would need to read the terminal's
-- answers, so this stays with the name the terminal reports.
supportsTextSizing :: IO Bool
supportsTextSizing =
    maybe False ("kitty" `isInfixOf`) <$> lookupEnv "TERM"

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
putColorDoc col line txt = do
    let txtLn = case line of
            NewLine -> txt `T.snoc` '\n'
            CurrentLine -> txt
    putDoc $ col $ pretty txtLn

putColorList :: [(Colorize, Text)] -> IO ()
putColorList = traverse_ (\(c, d) -> putColorDoc c CurrentLine d)

pprint :: IO (Maybe Char) -> Doc AnsiStyle -> IO ()
pprint readKey doc = do
    isTerminal <- hIsTerminalDevice stdout
    mSize <- Terminal.size
    let width = maybe 80 Terminal.width mSize
        height = maybe 24 Terminal.height mSize
        layoutOptions =
            defaultLayoutOptions{layoutPageWidth = AvailablePerLine width 1}
        output = renderStrict $ layoutSmart layoutOptions doc
    if isTerminal
        then paged readKey height (T.lines output)
        else TIO.putStr (T.snoc output '\n')

-- | Show lines in pages of the terminal height, one keypress per page. A
-- pager like @less@ cannot be used: it strips OSC 66 (the double-size query)
-- and does not know how much room the scaled line takes. Printing here lets
-- the terminal draw every line itself, and the first page stays on screen
-- instead of scrolling to the end of a long answer. Keys come through
-- @getInputChar@, so only printable ones arrive: Space pages, @q@ quits.
paged :: IO (Maybe Char) -> Int -> [Text] -> IO ()
paged readKey height = go . paginate (height - 1)
    where
        go [] = pure ()
        go [page] = traverse_ TIO.putStrLn page
        go (page : rest) = do
            traverse_ TIO.putStrLn page
            putColorDoc cyan CurrentLine "-- Space: next page, q: quit --"
            hFlush stdout
            key <- readKey
            TIO.putStr "\r\ESC[2K"
            case key of
                Just 'q' -> pure ()
                Just _ -> go rest
                Nothing -> pure ()

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
