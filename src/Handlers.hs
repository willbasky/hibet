module Handlers
       ( Title
       , directSearch
       , getDubs
       , makeTextMap
       , mapMaybeTuple
       , mergeWithNum
       , zipWithRaw
       , zipWithMap
       ) where

import           Data.Map (Map)
import           Data.Text (Text)
import           System.IO (hPrint, stderr)
import           Path (File, Abs, Path, filename,fromRelFile)

import           Prettify (greenCode, boldCode, blueCode, cyanCode, resetCode, putTextFlush)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO


type Title = Text

-- | Show duplicates and write to file.
getDubs :: IO ()
getDubs = do
    berzin <- IO.readFile "dics/03-Berzin"
    putTextFlush "Berzin file is loaded"
    let dubs = findDups berzin
    IO.writeFile "dics/dubs" $ T.pack $ show dubs
    hPrint stderr dubs

-- | Make Map from raw file. Merge duplicates to on key without delete
makeTextMap :: Text -> Map Text Text
makeTextMap
    = Map.fromAscListWithKey (\_ a1 a2 -> T.concat ["- ", a1, "\n- ", a2])
    . map ((\(y,x) -> (y, T.drop 1 x))
    . T.span (<'|'))
    . T.lines

-- | List tuples of duplicates only from raw file.
findDups :: Text -> [(Text,Text)]
findDups
    = dups
    . map ((\(y,x) -> (y, T.drop 1 x))
    . T.span (<'|'))
    . T.lines

-- | Get duplicate tuples only.
dups :: Eq k => [(k,v)] -> [(k,v)]
dups []     = []
dups (x:xs) =  d ++ dups r
  where
    (d,r) = (resultDub x, filter (\(y,_) -> fst x /= y) xs)
    isDub = filter (\(y,_) -> fst x == y) xs
    resultDub first | null isDub = []
                    | otherwise  = first : isDub

-- | Remove empty answers
mapMaybeTuple :: (a -> Maybe b) -> [(a,t)] -> [(b,t)]
mapMaybeTuple _ [] = []
mapMaybeTuple f ((x, t):xs) =
    let rs = mapMaybeTuple f xs in
    case (f x, t) of
        (Nothing, _) -> rs
        (Just r, n)  -> (r,n):rs

-- | Combine dictionary titles with mapped dictionaries.
zipWithMap :: [Text] -> [Path Abs File] -> [(Map Text Text, Title)]
zipWithMap texts files = zip mapped titles
  where
    mapped :: [Map Text Text]
    mapped = map makeTextMap texts

    titles :: [Text]
    titles = map (T.drop 3 . T.pack . fromRelFile . filename) files

-- | Combine answers with numbering for raw text.
zipWithRaw :: [Text] -> [Path Abs File] -> [(Text, Title)]
zipWithRaw texts files = zip texts titles
  where
    titles :: [Text]
    titles = map ((\x -> blueCode <> boldCode<> x <> resetCode) . T.drop 3 . T.pack . fromRelFile . filename) files

-- | Search in raw dictionary files.
directSearch :: Text -> [(Text, Title)] -> [(Text, Title)]
directSearch query = foldl (\ acc (x,y) -> if search x == "" then acc else (search x, y) : acc) []
  where
    search :: Text -> Text
    search
        = T.unlines
        . map (T.append (cyanCode <> "༔ " <> resetCode) . T.drop 1 . T.dropWhile (/= '|'))
        . filter (T.isPrefixOf (T.append query "|"))
        . T.lines


mergeWithNum :: [(Text, Text)] -> Text
mergeWithNum = T.unlines . zipWith flatten numbers
  where
    -- uncomment to use mapped data
    -- ascValues :: [(Text, Text)]
    -- ascValues = map (\(v,t) -> (T.unlines . reverse . T.lines $ v, t)) dscValues

    numbers :: [Text]
    numbers = map ((\x -> greenCode <> T.append (T.pack x) ". " <> resetCode) . show) [1::Int ..]

    flatten :: Text -> (Text, Text) -> Text
    flatten n (v, t) = T.append (T.append n (T.append t "\n")) v
