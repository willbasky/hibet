module Handlers
       ( getDubs
       , makeTextMap
       , mapMaybeTuple
       ) where

import           Data.Map (Map)
import           Data.Text.Lazy (Text)
import           System.IO (hPrint, stderr)

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as LIO


-- | Show duplicates and write to file.
getDubs :: IO ()
getDubs = do
    berzin <- LIO.readFile "dics/03-Berzin"
    LIO.hPutStrLn stderr "Berzin file is loaded"
    let dubs = findDups berzin
    LIO.writeFile "dics/dubs" $ TL.pack $ show dubs
    hPrint stderr dubs

-- | Make Map from raw file. Merge duplicates to on key without delete
makeTextMap :: Text -> Map Text Text
makeTextMap
    = Map.fromAscListWithKey (\_ a1 a2 -> TL.concat ["- ", a1, "\n- ", a2])
    . map ((\(y,x) -> (y, TL.drop 1 x))
    . TL.span (<'|'))
    . TL.lines

-- | List tuples of duplicates only from raw file.
findDups :: Text -> [(Text,Text)]
findDups
    = dups
    . map ((\(y,x) -> (y, TL.drop 1 x))
    . TL.span (<'|'))
    . TL.lines

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
