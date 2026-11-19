{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Label
    ( getLabels
    , Labels (..)
    , LabelFull (..)
    , Title (..)
    ) where

import Type (HibetError (..))

import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import qualified Toml

-- import qualified Debug.Trace as Debug

newtype Title = Title {unTitle :: Text}
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Show)
    deriving anyclass (NFData)

data LabelFull = LabelFull
    { path :: !Text
    , lfId :: !Int
    , label :: !Title
    , about :: !Text
    , available :: !Bool
    , source :: !Text
    , target :: !(Set.Set Text)
    , year :: !(Maybe Int)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

newtype Labels = Labels
    { labelTitles :: [LabelFull]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

getLabels :: FilePath -> BS.ByteString -> Either HibetError Labels
getLabels path fileContent =
    case TE.decodeUtf8' fileContent of
        Left err ->
            Left $ DataInvalid path (T.pack (show err))
        Right meta -> case Toml.decode labelsCodec meta of
            Left err ->
                Left $
                    DataInvalid
                        path
                        (T.intercalate "\n" (map Toml.prettyTomlDecodeError err))
            Right (Labels lbs) -> Right $ Labels $ sortOn lfId lbs

labelFullCodec :: TomlCodec LabelFull
labelFullCodec =
    LabelFull
        <$> Toml.text "path" .= path
        <*> Toml.int "id" .= lfId
        <*> Toml.diwrap (Toml.text "label") .= label
        <*> Toml.text "about" .= about
        <*> Toml.bool "available" .= available
        <*> Toml.text "source" .= source
        <*> Toml.arraySetOf Toml._Text "target" .= target
        <*> Toml.dioptional (Toml.int "year") .= year

labelsCodec :: TomlCodec Labels
labelsCodec =
    Labels
        <$> Toml.list labelFullCodec "titles" .= labelTitles
