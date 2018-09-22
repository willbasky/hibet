module Tibet
       ( cli
       ) where

import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO


cli :: IO ()
cli = do
    hPutStrLn stderr "What tibetan word to translate?"
    query <- hPutStr stderr "Put query: " >> getLine
    berzin <- IO.readFile "dics/03-Berzin"
    -- hPutStrLn stderr "Berzin file is loaded"
    let mapped = makeMap berzin
    -- hPutStrLn stderr "mapped dic is done"
    let value = fromMaybe "Nothing found" $ Map.lookup (T.pack query) mapped
    if T.null value then putStrLn "No query, no value"
    else hPutStrLn stderr $ T.unpack value

makeMap :: Text -> Map Text Text
makeMap raw = Map.fromList $ map ((\(y,x) -> (y, T.drop 1 x)) . T.span (<'|')) $ T.splitOn "\n" raw
