module Pretty
  (
  -- Colors
    blue
  , cyan
  , green
  , magenta
  , red
  , yellow
  -- Functions
  , putColorDoc
  , textToColorText
  , viewTranslations
  , withHeader
  , withHeaderSpaces
  ) where


import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc, annotate, defaultLayoutOptions, fillSep, hang, layoutSmart,
                                  pretty, space, vsep)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (..), bold, color, putDoc,
                                                  renderStrict)

import Types


-- | Header
withHeader :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader col header value =
    hang indentation $ vsep [col $ pretty header, value]

-- | Header with spaces
withHeaderSpaces :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> Doc AnsiStyle -> Doc AnsiStyle
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

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate $ color Red <> bold

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate $ color Green <> bold

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate $ color Blue <> bold

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate $ color Cyan <> bold

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate $ color Magenta <> bold

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate $ color Yellow <> bold

---------------------------------------------------------------------
-- Pretty functions
---------------------------------------------------------------------

-- | Pretty view translation.
viewTranslations :: [Answer] -> Doc AnsiStyle
viewTranslations = sparsedStack . map viewTranslation
  where
    viewTranslation :: Answer -> Doc AnsiStyle
    viewTranslation (value, (title, number)) =
      withHeader green (header number title) $ prettyTargets value
    -- Compose header
    header :: Int -> Target -> Text
    header number title = T.concat [T.pack $ show number, ". ", title]
    -- Decode value
    prettyTargets :: [Target] -> Doc AnsiStyle
    prettyTargets = vsep . map (wrapLines . fixNewLine)
    -- Fix new lines inside value
    fixNewLine :: Text -> Text
    fixNewLine = T.replace "\\n" "\n"

putColorDoc :: (Doc AnsiStyle -> Doc AnsiStyle) -> Line -> Text -> IO ()
putColorDoc col isNewLine txt =
  let txtLn = case isNewLine of
        NewLine     -> txt `T.snoc` '\n'
        CurrentLine -> txt
  in putDoc $ col $ pretty txtLn

textToColorText :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text  -> Text
textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt



