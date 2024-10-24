module Pretty
  (
  -- Colors
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
  ) where

import Dictionary (Target(..), Answer(..))
import Label (Title(..))
import Utility (showT)

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc, annotate, defaultLayoutOptions, fillSep, hang, layoutSmart, pretty,
                      space, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, renderStrict)


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
