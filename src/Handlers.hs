{-# LANGUAGE DeriveAnyClass #-}


module Handlers
       ( Title
       , Dictionary
       , DictionaryMeta (..)
       , Source
       , Target
       , makeTextMap
       , mergeWithNum
       , searchTranslation
       , selectDict
       , separator
       , sortOutput
       , toDictionaryMeta
       ) where


import Control.DeepSeq (NFData)
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath.Posix (takeBaseName)

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

import Labels (LabelFull (..))
import Parse (ParseError, Tibet, Wylie)
import Prettify (blue, bold, cyan, green)


type Title = Text
type Source = Text
type Target = Text

type Dictionary = HashMap Source Target -- | key and value

data DictionaryMeta = DictionaryMeta
  { dmDictionary :: Dictionary
  , dmTitle      :: Text
  , dmNumber     :: Int
  } deriving (Generic, NFData)

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeTextMap :: Text -> Dictionary
makeTextMap
    = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2])
    . map ((\(y,x) -> (y, T.drop 1 x)) . T.span (<'|'))
    . T.lines

-- | Select several dictionaries by id.
selectDict :: Maybe [Int] -> [DictionaryMeta] -> [DictionaryMeta]
selectDict mSelected dicts = case mSelected of
    Nothing         -> dicts
    Just selectedId -> filter (\DictionaryMeta{..} -> dmNumber `elem` selectedId) dicts

-- Add lables to dictionaries
toDictionaryMeta :: [LabelFull] -> FilePath -> Dictionary -> DictionaryMeta
toDictionaryMeta labels path dict = DictionaryMeta dict title number
  where
    (title, number) = findTitle $ takeBaseName path
    -- Match filpath with labels
    findTitle :: String -> (Title, Int)
    findTitle f = maybe ("Invalid title",0) (\LabelFull{..} -> (lfLabel, lfId))
      $ find (\LabelFull{..} -> T.pack f == lfPath) labels

-- Search query in dictionary.
searchTranslation :: Text -> DictionaryMeta -> Maybe ([Target], (Title, Int))
searchTranslation query DictionaryMeta{..} = if null ts then Nothing else Just (ts, (dmTitle, dmNumber))
  where
    ts = HMS.foldrWithKey search [] dmDictionary
    search :: Source -> Target -> [Target] -> [Target]
    search k v acc = if k == query then v : acc else acc

sortOutput :: [([Text], (Title, Int))] -> [([Text], (Title, Int))]
sortOutput = sortBy (\(_,(_,a)) (_,(_,b)) -> compare a b)

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
    valueMarked = T.unlines . map (\v -> cyan "► " <> insideNewLine v)
    -- Fix new lines inside value.
    insideNewLine :: Text -> Text
    insideNewLine = T.replace "\\n" "\n  "

-- Convert dictionaries from list to tibetan and pass others.
separator
  :: [Int]
  -> (Text -> Either ParseError [Tibet])
  -> ([Text], (Title, Int))
  -> Either ParseError ([Tibet], (Title, Int))
separator dictNumbers toTibetan d@(_, (_,i)) =
  if i `elem` dictNumbers then bitraverse (listToTibet toTibetan) pure d else Right d

listToTibet :: (Text -> Either ParseError [Tibet]) -> [Wylie] -> Either ParseError [Tibet]
listToTibet toTibetan list = do
  tibets <- traverse toTibetan list
  pure $ map (T.intercalate "\n" ) tibets
