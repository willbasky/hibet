module Handlers
       ( Title
       , Dictionary
       , searchInMap
       , makeTextMap
       , mergeWithNum
       , zipWithMap
       ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import           Path (Abs, File, Path, filename, fromRelFile)

import           Prettify (blueCode, boldCode, cyanCode, greenCode, resetCode)

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T


type Title = Text

type Dictionary = HashMap Text Text

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeTextMap :: Text -> Dictionary
makeTextMap
    = HMS.fromListWith (\a1 a2 -> T.concat [a1, "\n", a2])
    . map ((\(y,x) -> (y, T.drop 1 x))
    . T.span (<'|'))
    . T.lines

-- | Combine dictionary titles with mapped dictionaries.
zipWithMap :: [Text] -> [Path Abs File] -> [(Dictionary, Title)]
zipWithMap texts files = zip mapped titles
  where
    mapped :: [Dictionary]
    mapped = map makeTextMap texts

    titles :: [Title]
    titles = map (T.drop 3 . T.pack . fromRelFile . filename) files

-- Search in mapped dictionary.
searchInMap :: Text -> [(Dictionary, Title)] -> [(Text, Title)]
searchInMap query mapped = [(text, title) | (Just text, title) <- searched]
  where
    searched :: [(Maybe Text, Title)]
    searched = foldl (\ acc (x,y) -> if isNothing (search x) then acc else (search x, y) : acc) [] mapped

    search :: Dictionary -> Maybe Text
    search = HMS.lookup query

-- | Add numbers and flatten.
mergeWithNum :: [(Text, Title)] -> Text
mergeWithNum = T.unlines . zipWith flatten numbers
  where
    -- uncomment to use mapped data
    -- ascValues :: [(Text, Text)]
    -- ascValues = map (\(v,t) -> (T.unlines . reverse . T.lines $ v, t)) dscValues

    -- Add numbers.
    numbers :: [Text]
    numbers = map ((\x -> greenCode <> T.append (T.pack x) ". " <> resetCode) . show) [1::Int ..]

    flatten :: Text -> (Text, Title) -> Text
    flatten number (value, title) = T.append (T.append number (T.append (prettyT title) "\n")) (marked value)
    -- Paint title
    prettyT :: Title -> Title
    prettyT title = blueCode <> boldCode <> title <> resetCode

    marked value = T.unlines . map (\v -> cyanCode <> "༔ " <> resetCode <> v) $ T.lines value
