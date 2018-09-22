module Tibet
       ( cli
       ) where

import Data.Map (Map)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import qualified Data.Text.IO as IO
import qualified Data.Text as T
import qualified Data.Map as Map


cli :: IO ()
cli = do
    putStrLn "What tibetan word to translate?"
    query <- putStr "Put query: " >> getLine
    berzin <- IO.readFile "dics/03-Berzin"
    -- putStrLn "Berzin file is loaded"
    let mapped = makeMap berzin
    -- putStrLn "mapped dic is done"
    let value = fromMaybe "Nothing found" $ Map.lookup (T.pack query) mapped
    if T.null value then putStrLn "No query, no value"
    else putStrLn $ T.unpack value

makeMap :: Text -> Map Text Text
makeMap raw = Map.fromList $ map ((\(y,x) -> (y, T.drop 1 x)) . T.span (<'|')) $ T.splitOn "\n" raw
