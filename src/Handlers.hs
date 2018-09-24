module Handlers
       ( getDubs
       , makeTextMap
       , mapMaybeTuple
       ) where

import           Data.Map (Map)
import           Data.Text (Text)
import           System.IO (hPrint, stderr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO


-- | Show duplicates and write to file.
getDubs :: IO ()
getDubs = do
    berzin <- IO.readFile "dics/03-Berzin"
    IO.hPutStrLn stderr "Berzin file is loaded"
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

mapMaybeTuple :: (a -> Maybe b) -> [(a,t)] -> [(b,t)]
mapMaybeTuple _ [] = []
mapMaybeTuple f ((x, t):xs) =
    let rs = mapMaybeTuple f xs in
    case (f x, t) of
        (Nothing, _) -> rs
        (Just r, n)  -> (r,n):rs
