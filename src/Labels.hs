module Labels
       ( LabelFull(..)
       , labels
       ) where

import Data.List (sortOn)
import Data.Text (Text)
import Toml (TomlCodec, (.=))

import Hibet.Language

import qualified Data.Set as Set
import qualified Toml


labels :: Hibet [LabelFull]
labels = do
    file <- getDataFileNameH "stuff/titles.toml"
    meta <- getContentH file
    case Toml.decode labelsCodec meta of
        Left err               -> error $ show err
        Right (Labels decoded) -> pure $ sortOn lfId decoded

data LabelFull = LabelFull
    { lfPath      :: Text
    , lfId        :: Int
    , lfLabel     :: Text
    , lfAbout     :: Text
    , lfAvailable :: Bool
    , lfSource    :: Text
    , lfTarget    :: Set.Set Text
    , lfYear      :: Maybe Int
    } deriving (Eq, Show, Ord)

newtype Labels = Labels
    { labelTitles :: [LabelFull]
    } deriving (Eq, Show)

labelFullCodec :: TomlCodec LabelFull
labelFullCodec = LabelFull
    <$> Toml.text "path"  .= lfPath
    <*> Toml.int  "id"    .= lfId
    <*> Toml.text "label" .= lfLabel
    <*> Toml.text "about" .= lfAbout
    <*> Toml.bool "available" .= lfAvailable
    <*> Toml.text "source" .= lfSource
    <*> Toml.arraySetOf Toml._Text "target" .= lfTarget
    <*> Toml.dioptional (Toml.int "year") .= lfYear

labelsCodec :: TomlCodec Labels
labelsCodec = Labels
    <$> Toml.list labelFullCodec "titles" .= labelTitles
