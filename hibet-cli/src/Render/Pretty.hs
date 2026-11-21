module Render.Pretty
    ( -- Colors
      blue
    , cyan
    , green
    , magenta
    , red
    , white
    , yellow
    , Colorize
    -- Functions
    , textToColorText
    , viewTranslations
    , withHeader
    , withHeaderSpaces
    -- Output
    , Line (..)
    , keysRows
    , pageRows
    , paginate
    , pprint
    , putColorDoc
    , putColorDoc'
    , putColorList
    , terminalWidth
    , terminalHeight
    ) where

import Dictionary (Answer (..), Target (..))
import Label (Title (..))
import Render.Keys (Key (..), withRawKeys)
import Utility (showT)

import Control.Exception (bracket_)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.List (intersperse)
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
import System.IO (hFlush, hIsTerminalDevice, stdin, stdout)

-- | Header
withHeader :: Colorize -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader col header value =
    hang indentation $ vsep [col $ pretty header, value]

-- | Header with spaces
withHeaderSpaces :: Colorize -> Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
withHeaderSpaces col header value =
    hang indentation $ vsep [space, col header, space, value]

-- | Terminal width, 80 when the size is unknown.
terminalWidth :: IO Int
terminalWidth = maybe 80 Terminal.width <$> Terminal.size

-- | Terminal height, 24 when the size is unknown.
terminalHeight :: IO Int
terminalHeight = maybe 24 Terminal.height <$> Terminal.size

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

white :: Colorize
white = annotate $ color White <> bold

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

-- | Render the answer. When both stdin and stdout are terminals, the answers
-- are paged on the alternate screen with the query pinned above them; when the
-- pager closes the whole answer is printed once, so the query and the text stay
-- in the scrollback as they do without a pager. A redirected stream on either
-- end keeps the plain output: the pager draws to stdout and reads keys from
-- stdin, so it cannot run without both.
pprint :: Doc AnsiStyle -> Doc AnsiStyle -> IO ()
pprint header doc = do
    isTerminal <- (&&) <$> hIsTerminalDevice stdout <*> hIsTerminalDevice stdin
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
        then paged width height headerLines bodyLines output
        else TIO.putStr (T.snoc output '\n')

-- | Show the answers a window at a time with the header pinned above. The
-- window lives on the alternate screen, so it leaves no copies behind; on exit
-- the full answer is printed to the normal screen once, and the scrollback
-- holds the query and the text as without a pager. Keys are read raw by
-- "Render.Keys", so arrows and PgUp/PgDn work;
-- @j@/@k@ and Ctrl-N/Ctrl-P move a line, Space/@b@, PgUp/PgDn and
-- Ctrl-F/Ctrl-B or Ctrl-V/Alt-V move a page, @q@ quits.
paged :: Int -> Int -> [Text] -> [Text] -> Text -> IO ()
paged width height header body output =
    case paginate pageSize body of
        [] -> TIO.putStr (T.snoc output '\n')
        [_] -> TIO.putStr (T.snoc output '\n')
        _ -> do
            bracket_
                (TIO.putStr "\ESC[?1049h" >> hFlush stdout)
                (TIO.putStr "\ESC[?1049l" >> hFlush stdout)
                $ withRawKeys
                $ \readKey -> do
                    draw 0
                    loop readKey 0
            TIO.putStr (T.snoc output '\n')
    where
        pageSize = max 1 (height - keysRows width - pageRows header)
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

-- | Rows a page takes on screen.
pageRows :: [Text] -> Int
pageRows = length

-- | Keys that work from this offset.
keys :: Int -> Int -> Text
keys offset lastStart
    | offset <= 0 = keysStart
    | offset >= lastStart = keysEnd
    | otherwise = keysMiddle

keysStart, keysEnd, keysMiddle :: Text
keysStart = "-- ↓/j: down  PgDn/Space: page down  q: quit --"
keysEnd = "-- ↑/k: up  PgUp/b: page up  q: quit --"
keysMiddle = "-- ↑/k: up  ↓/j: down  PgUp/b, PgDn/Space: page  q: quit --"

-- | Rows the keys hint takes in a window @width@ cells wide. The longest hint
-- is reserved whatever the offset, so the page size does not change as the
-- pager moves between the shorter hints.
keysRows :: Int -> Int
keysRows width = max 1 ((longestHint + width' - 1) `div` width')
    where
        width' = max 1 width
        longestHint = maximum (map T.length [keysStart, keysEnd, keysMiddle])

-- | Split rendered lines into pages of at most @size@ terminal rows.
paginate :: Int -> [Text] -> [[Text]]
paginate size = go
    where
        n = max 1 size
        go [] = []
        go lines' =
            let (page, rest) = splitAt n lines'
             in page : go rest
