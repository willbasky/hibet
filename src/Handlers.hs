module Handlers
       ( Title
       , Dictionary
       , History
       , makeTextMap
       , mergeWithNum
       , searchInMap
       , selectDict
       , zipWithMap
       ) where

import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Maybe (isNothing, maybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Path (Abs, File, Path, filename, fromRelFile)

import Labels (LabelFull (..))
import Prettify (blue, bold, cyan, green)

import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T


type Title = ByteString

type Dictionary = HashMap ByteString ByteString -- | key and value

type History = StateT [ByteString] IO [ByteString] -- | first value is state

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeTextMap :: ByteString -> Dictionary
makeTextMap
    = HMS.fromListWith (\a1 a2 -> if a1 == a2 then a1 else BC.concat [a1, "\n", a2])
    . map ((\(y,x) -> (y, BC.drop 1 x))
    . BC.span (<'|'))
    . BC.lines

-- | Select several dictionaries by id.
selectDict :: Maybe [Int] -> [(Dictionary, (Title, Int))] -> [(Dictionary, (Title, Int))]
selectDict mSelected dicts = case mSelected of
    Nothing         -> dicts
    Just selectedId -> filter (\(_, (_, i)) -> i `elem` selectedId) dicts

-- | Combine dictionary titles with mapped dictionaries.
zipWithMap :: [ByteString] -> [Path Abs File] -> [LabelFull] -> [(Dictionary, (Title, Int))]
zipWithMap texts files labels = zip mapped (titles labels)
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
        findTitle f = maybe ("Invalid title",0) (\LabelFull{..} -> (encodeUtf8 lfLabel, lfId)) $
            find (\LabelFull{..} -> f == lfPath) labels'

-- Search in mapped dictionary.
searchInMap :: ByteString -> [(Dictionary, (Title, Int))] -> [(ByteString, (Title, Int))]
searchInMap query mapped =
    sortBy (\(_,(_,a)) (_,(_,b)) -> compare a b)
        [(text, (title, number)) | (Just text, (title, number)) <- searched]
  where
    searched :: [(Maybe ByteString, (Title, Int))]
    searched = foldl (\ acc (x,(t,i)) -> if isNothing (search x) then acc else (search x, (t,i)) : acc) [] mapped

    search :: Dictionary -> Maybe ByteString
    search = HMS.lookup query

-- | Add numbers and flatten.
mergeWithNum :: [(ByteString, (Title, Int))] -> Text
mergeWithNum = T.intercalate "\n" . map flatten
  where
    -- Prettify number.
    prettyN :: Int -> Text
    prettyN = ((\x -> green $ T.append (T.pack x) ". ") . show)

    flatten :: (ByteString, (Title, Int)) -> Text
    flatten (value, (title, number)) =
        T.append (T.append (prettyN number) (T.append (prettyT title) "\n")) (valueMarked value)
    -- Decode and paint title.
    prettyT :: Title -> Text
    prettyT title = blue $ bold $ decodeUtf8 title
    -- Decode value and add mark.
    valueMarked :: ByteString -> Text
    valueMarked value =
        T.unlines . map (\v -> cyan "► " <> insideNewLine v) $ T.lines (decodeUtf8 value)
    -- Fix new lines inside value.
    insideNewLine :: Text -> Text
    insideNewLine = T.replace "\\n" "\n  "

