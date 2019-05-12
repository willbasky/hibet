{-# LANGUAGE DeriveAnyClass #-}

module Handlers
       ( Title
       , Dictionary
       , History
       , DictionaryMeta
       , makeTextMap
       , mergeWithNum
       , searchInMap
       , selectDict
       , zipWithMap
       ) where

import Control.DeepSeq
import Control.Monad.Trans.State.Strict (StateT)
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Maybe (mapMaybe, maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, File, Path, filename, fromRelFile)

import Labels (LabelFull (..))
import Prettify (blue, bold, cyan, green)

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T


type Title = Text

type Dictionary = HashMap Text Text -- | key and value

data DictionaryMeta = DictionaryMeta
  { dmDictionary :: Dictionary
  , dmTitle      :: Text
  , dmNumber     :: Int
  }
  deriving (Generic, NFData)

type History = StateT [Text] IO [Text] -- | first value is state

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeTextMap :: Text -> Dictionary
makeTextMap
    = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2])
    . map ((\(y,x) -> (y, T.drop 1 x))
    . T.span (<'|'))
    . T.lines

-- | Select several dictionaries by id.
selectDict :: Maybe [Int] -> [DictionaryMeta] -> [DictionaryMeta]
selectDict mSelected dicts = case mSelected of
    Nothing         -> dicts
    Just selectedId -> filter (\DictionaryMeta{..} -> dmNumber `elem` selectedId) dicts

-- | Combine dictionary titles with mapped dictionaries.
zipWithMap :: [Text] -> [Path Abs File] -> [LabelFull] -> [DictionaryMeta]
zipWithMap texts files labels = zipWith (\d (t, i) -> DictionaryMeta d t i) mapped (titles labels)
  where
    mapped :: [Dictionary]
    mapped = map makeTextMap texts

    titles :: [LabelFull] -> [(Title, Int)]
    titles labels' = map findTitle filepathes
      where
        -- Trim filepath
        filepathes :: [Text]
        filepathes = map (T.dropEnd 4 . T.pack . fromRelFile . filename) files
        -- Match filpath with labels
        findTitle :: Text -> (Title, Int)
        findTitle f = maybe ("Invalid title",0) (\LabelFull{..} -> (lfLabel, lfId)) $
            find (\LabelFull{..} -> f == lfPath) labels'

-- Search in mapped dictionary.
searchInMap :: Text -> [DictionaryMeta] -> [([Text], (Title, Int))]
searchInMap query mapped =
    sortBy (\(_,(_,a)) (_,(_,b)) -> compare a b) (searched mapped)
  where
    searched :: [DictionaryMeta] -> [([Text], (Title, Int))]
    searched = mapMaybe (\DictionaryMeta{..} -> notNull (values dmDictionary, (dmTitle, dmNumber)))

    search k v acc = if k == query then v : acc else acc

    values = HMS.foldrWithKey search []

    notNull (dict, meta) = case dict of
      [] -> Nothing
      _  -> Just (dict, meta)


-- | Add numbers and flatten.
mergeWithNum :: [([Text], (Title, Int))] -> Text
mergeWithNum = T.intercalate "\n" . map flatten
  where
    -- Prettify number.
    prettyN :: Int -> Text
    prettyN = (\x -> green $ T.append (T.pack x) ". ") . show

    flatten :: ([Text], (Title, Int)) -> Text
    flatten (value, (title, number)) =
        T.concat [prettyN number, prettyT title, "\n", valueMarked value]
    -- Decode and paint title.
    prettyT :: Title -> Text
    prettyT title = blue $ bold title
    -- Decode value and add mark.
    valueMarked :: [Text] -> Text
    valueMarked value = T.unlines $ map (\v -> cyan "► " <> insideNewLine v) value
    -- Fix new lines inside value.
    insideNewLine :: Text -> Text
    insideNewLine = T.replace "\\n" "\n  "

