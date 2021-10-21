{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module Dictionary
       ( Title
       , Dictionary
       , DictionaryMeta (..)
       , Source
       , Target
       , Answer
       , makeDictionary
       , searchTranslation
       , selectDict
       , separator
       , sortOutput
       , toDictionaryMeta
       ) where

import Parse
import Labels

import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (find)
import Data.List (sortBy)
import Data.Text (Text)
import System.FilePath.Posix (takeBaseName)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap)

type Title = Text
type Source = Text
type Target = Text
type Answer = ([Target], (Title, Int))

type Dictionary = HashMap Source Target -- key and value

data DictionaryMeta = DictionaryMeta
  { dictionary :: !Dictionary
  , title      :: !Text
  , number     :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeDictionary :: Text -> Dictionary
makeDictionary
    = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2])
    . map (second (T.drop 1) . T.span (<'|'))
    . T.lines

-- | Select several dictionaries by id.
selectDict :: [Int] -> [DictionaryMeta] -> [DictionaryMeta]
selectDict selected dicts = case selected of
    []          -> dicts
    selectedIds -> filter (\dm -> dm.number `elem` selectedIds) dicts

-- Add lables to dictionaries
toDictionaryMeta :: [LabelFull] -> FilePath -> Dictionary -> DictionaryMeta
toDictionaryMeta labels filepath dict = DictionaryMeta dict title number
  where
    (title, number) = findTitle $ T.pack $ takeBaseName filepath
    -- Match filpath with labels
    findTitle :: Text -> (Title, Int)
    findTitle path = maybe ("Invalid title",0) (\lf -> (lf.label, lf.lfId))
      $ find (\lf -> path == lf.path) labels

-- Search query in dictionary.
searchTranslation :: Text -> DictionaryMeta -> Maybe Answer
searchTranslation query dm =
  if null ts then Nothing else Just (ts, (dm.title, dm.number))
  where
    ts = HMS.foldrWithKey search [] dm.dictionary
    search :: Source -> Target -> [Target] -> [Target]
    search k v acc = if k == query then v : acc else acc

sortOutput :: [Answer] -> [Answer]
sortOutput = sortBy (\(_,(_,a)) (_,(_,b)) -> compare a b)

-- Convert dictionaries from list to tibetan and pass others.
separator
  :: [Int]
  -> (Text -> Except ParseError [Tibet])
  -> ([Text], (Title, Int))
  -> Except ParseError ([Tibet], (Title, Int))
separator dictNumbers toTibetan' d@(_, (_,i)) =
  if i `elem` dictNumbers then bitraverse (listToTibet toTibetan') pure d else pure d

listToTibet :: (Text -> Except ParseError [Tibet]) -> [Wylie] -> Except ParseError [Tibet]
listToTibet toTibetan' list = do
  tibets <- traverse toTibetan' list
  pure $ map (T.intercalate "\n" ) tibets
