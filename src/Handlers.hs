module Handlers
       ( Title
       , Dictionary
       , searchInRaw
       , searchInMap
       , makeTextMap
       , mergeWithNum
       , zipWithRaw
       , zipWithMap
       ) where

import           Data.Map (Map)
import           Data.Text (Text)
import           Path (File, Abs, Path, filename,fromRelFile)
import           Data.Maybe (isNothing)

import           Prettify (greenCode, boldCode, blueCode, cyanCode, resetCode)

import qualified Data.Map as Map
import qualified Data.Text as T


type Title = Text

type Dictionary = Map Text Text

-- | Make Map from raw file. Merge duplicates to on key without delete.
makeTextMap :: Text -> Dictionary
makeTextMap
    = Map.fromAscListWithKey (\_ a1 a2 -> T.concat [dub, a1, "\n", dub, a2])
    . map ((\(y,x) -> (y, T.drop 1 x))
    . T.span (<'|'))
    . T.lines
  where
    dub = (cyanCode <> "༔ " <> resetCode)

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
    search = Map.lookup query

-- | Combine answers with numbering for raw text.
zipWithRaw :: [Text] -> [Path Abs File] -> [(Text, Title)]
zipWithRaw texts files = zip texts titles
  where
    titles :: [Text]
    titles = map (T.drop 3 . T.pack . fromRelFile . filename) files

-- | Search in raw dictionary files.
searchInRaw :: Text -> [(Text, Title)] -> [(Text, Title)]
searchInRaw query = foldl (\ acc (x,y) -> if search x == "" then acc else (search x, y) : acc) []
  where
    search :: Text -> Text
    search
        = T.unlines
        . map (T.append (cyanCode <> "༔ " <> resetCode) . T.drop 1 . T.dropWhile (/= '|'))
        . filter (T.isPrefixOf (T.append query "|"))
        . T.lines

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
    flatten n (v, t) = T.append (T.append n (T.append (prettyT t) "\n")) v
    -- Paint title
    prettyT :: Title -> Title
    prettyT t = blueCode <> boldCode <> t <> resetCode
