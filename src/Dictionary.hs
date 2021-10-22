{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Dictionary
       ( Title
       , Dictionary
       , DictionaryMeta (..)
       , Target(..)
       , Answer(..)
       , makeDictionary
       , searchTranslation
       , selectDict
       , separator
       , sortOutput
       , toDictionaryMeta
       ) where

import Label (Title(..), LabelFull(..))
import Parse (ParseError, Tibet, Wylie)

import Control.Monad.Except (Except)
import Control.Parallel.Strategies (NFData)
import Data.Bifunctor (second)
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath.Posix (takeBaseName)


newtype Target = Target {unTarget :: Text}
  deriving stock (Eq, Generic, Ord)
  deriving newtype (Show)
  deriving anyclass (NFData)

data Answer = Answer
  { targets    :: ![Target]
  , dictNumber :: !Int
  , dictTitle  :: !Title
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- Kee = unSource source
type Dictionary = HashMap Text [Target]

data DictionaryMeta = DictionaryMeta
  { dictionary :: !Dictionary
  , title      :: !Title
  , number     :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeDictionary :: Text -> Dictionary
makeDictionary
    = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else a1 <> a2)
    . map (second ((:[]) . Target . T.drop 1) . T.span (<'|'))
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
    findTitle path = maybe (Title "Invalid title",0) (\lf -> (lf.label, lf.lfId))
      $ find (\lf -> path == lf.path) labels

-- Search query in dictionary.
searchTranslation :: Text -> DictionaryMeta -> Maybe Answer
searchTranslation query dm =
  if null ts then Nothing else Just $ Answer ts dm.number dm.title
  where
    ts = HMS.foldrWithKey search [] dm.dictionary
    search :: Text -> [Target] -> [Target] -> [Target]
    search q v acc = if q == query then v <> acc else acc

sortOutput :: [Answer] -> [Answer]
sortOutput = sortBy (\a1 a2-> compare a1.dictNumber a2.dictNumber)

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
