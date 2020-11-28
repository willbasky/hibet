{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Dictionary
       ( Title
       , Dictionary
       , DictionaryMeta (..)
       , Source
       , Target
       , makeTextMap
       , searchTranslation
       , selectDict
       , separator
       , toDictionaryMeta
       , getAnswer
       ) where

import Parse
import Pretty
import Types

import Control.Monad.Except
import Control.Parallel.Strategies
import Data.Bifunctor (second)
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (find)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Data.Vector (Vector)
-- import Debug.Trace
import System.FilePath.Posix (takeBaseName)

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Vector as V


getAnswer :: Query -> Env -> Except ParseError (Doc AnsiStyle, Bool)
getAnswer query env = do
  let toWylie' = toWylie env.tibetWylie . parseTibetanInput env.radixTibet
      queryWylie = case runExcept $ toWylie' query  of
        Left _      -> query
        Right wylie -> if T.null wylie then query else wylie
      dscValues = V.mapMaybe (searchTranslation queryWylie) env.dictionaryMeta `using` parTraversable rseq
  let toTibetan' = toTibetan env.wylieTibet . parseWylieInput env.radixWylie
  -- list <- traverse (separator [37] toTibetan') dictMeta
  let (translations, isEmpty) = (viewTranslations $ V.toList dscValues, V.null dscValues)
  query' <- if query == queryWylie
    then T.concat <$> toTibetan' queryWylie
    else pure queryWylie
  pure (withHeaderSpaces yellow query' translations, isEmpty)

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeTextMap :: Text -> Dictionary
makeTextMap
    -- = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else T.concat [a1, "\n", a2]) -- O(n*log n)
    = HMS.fromList -- O(n)
    . map (second (T.drop 1) . T.span (<'|'))
    . T.lines

-- | Select several dictionaries by id.
selectDict :: [Int] -> Vector DictionaryMeta -> Vector DictionaryMeta
selectDict selected dicts = case selected of
    []          -> dicts
    selectedIds -> V.filter (\dm -> dm.number `elem` selectedIds) dicts

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
