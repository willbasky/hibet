{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dictionary
    ( Title
    , Dictionary
    , DictionaryMeta (..)
    , Target (..)
    , Answer (..)
    , makeDictionary
    , addAliases
    , searchTranslation
    , selectDict
    , sortOutput
    , toDictionaryMeta
    ) where

import Label (LabelFull (..), Title (..))

import Control.Parallel.Strategies (NFData)
import Data.Bifunctor (second)
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lines as Line
import GHC.Generics (Generic)
import System.FilePath.Posix (takeBaseName)

newtype Target = Target {unTarget :: Text}
    deriving stock (Eq, Generic, Ord)
    deriving newtype (Show)
    deriving anyclass (NFData)

data Answer = Answer
    { targets :: ![Target]
    , dictNumber :: !Int
    , dictTitle :: !Title
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

-- Kee = unSource source
type Dictionary = HashMap Text [Target]

data DictionaryMeta = DictionaryMeta
    { dictionary :: !Dictionary
    , title :: !Title
    , number :: !Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeDictionary :: Text -> Dictionary
makeDictionary =
    HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else a1 <> a2)
        . map (second ((: []) . Target . T.drop 1) . T.span (< '|'))
        . Line.lines
        . Line.fromText

-- | Add the canonical spelling of a key next to the key itself, with the same
-- targets. Dictionary keys keep the spelling of their source, while a Unicode
-- query goes through hewts and gets canonical Wylie; the alias closes the gap
-- without touching the dictionaries. The legacy key stays: Wylie users keep
-- typing it as before.
addAliases :: HashMap Text Text -> Dictionary -> Dictionary
addAliases aliases dict = HMS.foldlWithKey' add dict dict
    where
        add acc key targets = case HMS.lookup key aliases of
            Just canonical
                | canonical /= key ->
                    HMS.insertWith merge canonical targets acc
            _ -> acc
        merge new old = if new == old then new else new <> old

-- | Select several dictionaries by id.
selectDict :: [Int] -> [DictionaryMeta] -> [DictionaryMeta]
selectDict selected dicts = case selected of
    [] -> dicts
    selectedIds -> filter (\dm -> dm.number `elem` selectedIds) dicts

-- Add lables to dictionaries
toDictionaryMeta :: [LabelFull] -> FilePath -> Dictionary -> DictionaryMeta
toDictionaryMeta labels filepath dict = DictionaryMeta dict title number
    where
        (!title, !number) = findTitle $ T.pack $ takeBaseName filepath
        -- Match filpath with labels
        findTitle :: Text -> (Title, Int)
        findTitle path =
            maybe (Title "Invalid title", 0) (\lf -> (lf.label, lf.lfId)) $
                find (\lf -> path == lf.path) labels

-- Search query in dictionary.
searchTranslation :: Text -> DictionaryMeta -> Maybe Answer
searchTranslation query dm =
    case HMS.lookup query dm.dictionary of
        Just ts | not (null ts) -> Just $ Answer ts dm.number dm.title
        _ -> Nothing

sortOutput :: [Answer] -> [Answer]
sortOutput = sortBy (\a1 a2 -> compare a1.dictNumber a2.dictNumber)
