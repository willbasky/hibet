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
    , pprint
    , putColorDoc
    , putColorList
    ) where

import Dictionary (Answer (..), Target (..))
import Label (Title (..))
import Utility (showT)

import Control.Monad (when)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.List (intersperse)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
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
import System.Environment (lookupEnv, setEnv)
import System.Pager (printOrPage)

-- | Header
withHeader :: Colorize -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader col header value =
    hang indentation $ vsep [col $ pretty header, value]

-- | Header with spaces
withHeaderSpaces :: Colorize -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeaderSpaces col header value =
    hang indentation $ vsep [space, col $ pretty header, space, value]

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

pprint :: Doc AnsiStyle -> IO ()
pprint doc = do
    -- enable colors in `less`
    lessConf <- lookupEnv "LESS"
    when (isNothing lessConf) $ setEnv "LESS" "-R"
    width' <- maybe 80 Terminal.width <$> Terminal.size
    let layoutOptions =
            defaultLayoutOptions{layoutPageWidth = AvailablePerLine width' 1}
    printOrPage . (`T.snoc` '\n') . renderStrict $ layoutSmart layoutOptions doc
