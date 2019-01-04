module Labels
       ( LabelFull(..)
       , labels
       ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, object, withObject, (.:), (.=))
import Data.List (sortOn)
import Data.Text (Text)

import Paths_tibet (getDataFileName)

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM


labels :: IO [LabelFull]
labels = do
    file <- getDataFileName "/titles.json"
    meta <- BLC.readFile file
    case decode meta :: Maybe Labels of
        Nothing               -> error "Not decoded"
        Just (Labels decoded) -> pure $ sortOn lfId decoded

data Label = Label
    { labelId    :: Int
    , labelLabel :: Text
    , labelMeta  :: Text
    } deriving (Eq, Show, Ord)

instance ToJSON Label where
    toJSON Label{..} = object
        [ "id"     .= labelId
        , "label"  .= labelLabel
        , "about"  .= labelMeta
        ]

instance FromJSON Label where
    parseJSON = withObject "label" $ \v -> Label
        <$> v .: "id"
        <*> v .: "label"
        <*> v .: "about"

data LabelFull = LabelFull
    { lfPath  :: Text
    , lfId    :: Int
    , lfLabel :: Text
    , lfMeta  :: Text
    } deriving (Eq, Show, Ord)

newtype Labels = Labels [LabelFull]
    deriving (Eq, Show)

instance FromJSON Labels where
    parseJSON v
        = fmap ( Labels
        . map (\(path, Label number label about) -> LabelFull path number label about)
        . HM.toList )
        $ parseJSON v
