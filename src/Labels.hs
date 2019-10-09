module Labels
       ( LabelFull(..)
       , labels
       ) where

import Data.List (sortOn)
import Data.Text (Text)
import Toml (TomlCodec, (.=))

import Paths_Hibet (getDataFileName)

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Toml


labels :: IO [LabelFull]
labels = do
    file <- getDataFileName "stuff/titles.toml"
    meta <- TE.decodeUtf8 <$> BS.readFile file
    case Toml.decode labelsCodec meta of
        Left err               -> error $ show err
        Right (Labels decoded) -> pure $ sortOn lfId decoded

data LabelFull = LabelFull
    { lfPath  :: Text
    , lfId    :: Int
    , lfLabel :: Text
    , lfMeta  :: Text
    } deriving (Eq, Show, Ord)

newtype Labels = Labels
    { labelTitles :: [LabelFull]
    } deriving (Eq, Show)

labelFullCodec :: TomlCodec LabelFull
labelFullCodec = LabelFull
    <$> Toml.text "path"  .= lfPath
    <*> Toml.int  "id"    .= lfId
    <*> Toml.text "label" .= lfLabel
    <*> Toml.text "about" .= lfMeta

labelsCodec :: TomlCodec Labels
labelsCodec = Labels
    <$> Toml.list labelFullCodec "titles" .= labelTitles
