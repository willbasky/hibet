module Titles
       ( Title(..)
       , showTitles
       ) where

import           Data.Aeson
import           Data.Text (Text)

import           Prettify (blueCode, putTextFlush, resetCode)
import           Paths_tibet (getDataFileName)

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM

showTitles :: IO ()
showTitles = do
    file <- getDataFileName "/titles.json"
    titles <- BLC.readFile file
    case decode titles :: Maybe TitleIdList of
        Nothing -> error "Not decoded"
        Just (TitleIdList decoded) ->
            mapM_ (\title -> putTextFlush (blueCode <> "- " <> tiLabel title <> resetCode)) decoded

data Title = Title
    { titleLabel :: Text
    , titleAbout :: Text
    } deriving (Eq, Show)

instance ToJSON Title where
    toJSON Title{..} = object
        [ "label"  .= titleLabel
        , "about"  .= titleAbout
        ]

instance FromJSON Title where
    parseJSON = withObject "title" $ \v -> Title
        <$> v .: "label"
        <*> v .: "about"

data TitleId = TitleId
    { tiPath  :: Text
    , tiLabel :: Text
    , tiAbout :: Text
    } deriving (Eq, Show)

newtype TitleIdList = TitleIdList [TitleId]
    deriving (Eq, Show)

instance FromJSON TitleIdList where
    parseJSON v
        = fmap ( TitleIdList
        . map (\(path, Title label about) -> TitleId path label about)
        . HM.toList )
        $ parseJSON v


