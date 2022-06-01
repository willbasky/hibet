module Common where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir, embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import Paths_hibet (getDataFileName)

pathLoad :: IO [(BS.ByteString, FilePath)]
pathLoad = do
  dir <- getDataFileName "dicts"
  dirAbs <- parseAbsDir dir
  files <- snd <$> listDir dirAbs
  let paths = map fromAbsFile files
  mapM (\p -> (,p) <$> BS.readFile p) paths

textLoad :: IO [Text]
textLoad = do
  dir <- getDataFileName "dicts"
  dirAbs <- parseAbsDir dir
  files <- snd <$> listDir dirAbs
  let paths = map fromAbsFile files
  mapM (fmap TE.decodeUtf8 . BS.readFile) paths

syllables :: Text
syllables = decodeUtf8 $(embedFile "stuff/tibetan-syllables")

titles :: Text
titles = decodeUtf8 $(embedFile "stuff/titles.toml")

dictinaries :: [(FilePath, ByteString)]
dictinaries = $(embedDir "dicts")
