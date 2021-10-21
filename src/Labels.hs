{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Labels
       ( getLabels
       , Labels(..)
       , LabelFull(..)
       , Title(..)
       ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import qualified Toml

newtype Title = Title Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show)
  deriving anyclass (NFData)

data LabelFull = LabelFull
    { path      :: !Text
    , lfId      :: !Int
    , label     :: !Title
    , about     :: !Text
    , available :: !Bool
    , source    :: !Text
    , target    :: !(Set.Set Text)
    , year      :: !(Maybe Int)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

newtype Labels = Labels
    { labelTitles :: [LabelFull]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

getLabels :: BS.ByteString -> Labels
getLabels fileContent = do
    let meta = TE.decodeUtf8 fileContent
    case Toml.decode labelsCodec meta of
        Left err           -> error $ show err
        Right (Labels lbs) -> Labels $ sortOn lfId lbs

labelFullCodec :: TomlCodec LabelFull
labelFullCodec = LabelFull
    <$> Toml.text "path"  .= path
    <*> Toml.int  "id"    .= lfId
    <*> Toml.diwrap (Toml.text "label") .= label
    <*> Toml.text "about" .= about
    <*> Toml.bool "available" .= available
    <*> Toml.text "source" .= source
    <*> Toml.arraySetOf Toml._Text "target" .= target
    <*> Toml.dioptional (Toml.int "year") .= year

labelsCodec :: TomlCodec Labels
labelsCodec = Labels
    <$> Toml.list labelFullCodec "titles" .= labelTitles
