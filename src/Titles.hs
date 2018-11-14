module Titles
       ( LabelFull(..)
       , labels
       ) where

import           Data.Aeson
import           Data.Text (Text)

import           Paths_tibet (getDataFileName)

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM

labels :: IO [LabelFull]
labels = do
    file <- getDataFileName "/titles.json"
    titles <- BLC.readFile file
    case decode titles :: Maybe Labels of
        Nothing               -> error "Not decoded"
        Just (Labels decoded) -> pure decoded

data Label = Label
    { tLabel :: Text
    , tAbout :: Text
    } deriving (Eq, Show)

instance ToJSON Label where
    toJSON Label{..} = object
        [ "label"  .= tLabel
        , "about"  .= tAbout
        ]

instance FromJSON Label where
    parseJSON = withObject "label" $ \v -> Label
        <$> v .: "label"
        <*> v .: "about"

data LabelFull = LabelFull
    { tiPath  :: Text
    , tiLabel :: Text
    , tiAbout :: Text
    } deriving (Eq, Show)

newtype Labels = Labels [LabelFull]
    deriving (Eq, Show)

instance FromJSON Labels where
    parseJSON v
        = fmap ( Labels
        . map (\(path, Label label about) -> LabelFull path label about)
        . HM.toList )
        $ parseJSON v


