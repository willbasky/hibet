module Pretty (
       -- Colors
       blue,
       cyan,
       green,
       magenta,
       red,
       yellow,
       -- Functions
       pprint,
       putColorDoc,
       putColorDocs,
       textToColorText,
       viewTranslations,
       withHeader,
       withHeaderSpaces
) where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Maybe (isNothing)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, LayoutOptions (..), PageWidth (..), annotate,
                                  defaultLayoutOptions, fillSep, hang, layoutSmart, pretty, space,
                                  vsep)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, Color (..), bold, color, putDoc,
                                                  renderStrict)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (lookupEnv, setEnv)
import System.Pager (printOrPage)

import Handlers (Target, Title)


-- | Header.
withHeader :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeader col header value =
    hang indentation $ vsep [col $ pretty header, value]

-- | Header with spaces.
withHeaderSpaces :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> Doc AnsiStyle -> Doc AnsiStyle
withHeaderSpaces col header value =
    hang indentation $ vsep [space, col $ pretty header, space, value]

indentation :: Int
indentation = 2

wrapLines :: Text -> Doc ann
wrapLines =
    vsep . map (fillSep . map pretty . Text.split isSpace) . Text.splitOn "\n"

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
viewTranslations :: [([Target], (Title, Int))] -> Doc AnsiStyle
viewTranslations = sparsedStack . map viewTranslation
  where
    viewTranslation :: ([Target], (Title, Int)) -> Doc AnsiStyle
    viewTranslation (value, (title, number)) =
      withHeader green (header number title) $ prettyTargets value
    -- Compose header
    header :: Int -> Target -> Text
    header number title = Text.concat [Text.pack $ show number, ". ", title]
    -- Decode value
    prettyTargets :: [Target] -> Doc AnsiStyle
    prettyTargets = vsep . map (wrapLines . fixNewLine)
    -- Fix new lines inside value
    fixNewLine :: Text -> Text
    fixNewLine = Text.replace "\\n" "\n"

pprint :: Doc AnsiStyle -> IO ()
pprint doc = do
  -- enable colors in `less`
  lessConf <- lookupEnv "LESS"
  when (isNothing lessConf) $ setEnv "LESS" "-R"
  width' <- maybe 80 Terminal.width <$> Terminal.size
  let layoutOptions =
        defaultLayoutOptions {layoutPageWidth = AvailablePerLine width' 1}
  printOrPage . (`Text.snoc` '\n') . renderStrict $ layoutSmart layoutOptions doc

putColorDoc :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text -> IO ()
putColorDoc col txt = putDoc $ col $ pretty (txt `Text.snoc` '\n')

putColorDocs :: [(Doc AnsiStyle -> Doc AnsiStyle, Text)] -> IO ()
putColorDocs docs = do
  mapM_ (\(col,txt) -> putDoc $ col $ pretty txt) docs
  putStrLn ""

textToColorText :: (Doc AnsiStyle -> Doc AnsiStyle) -> Text  -> Text
textToColorText col txt = renderStrict $ layoutSmart defaultLayoutOptions $ col $ pretty txt



