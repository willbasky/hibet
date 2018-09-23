module Tibet
       ( start
       ) where

import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy (Text)
import           System.IO (hPutStr, hPutStrLn, stderr)
-- import           Data.ByteString.Lazy (ByteString)

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as IO
-- import qualified Data.ByteString.Lazy as BL

start :: IO ()
start = do
    hPutStrLn stderr "What tibetan word to translate?"
    cli

cli :: IO ()
cli = do
    query <- hPutStr stderr "> " >> getLine
    berzin <- IO.readFile "dics/03-Berzin"
    -- hPutStrLn stderr "Berzin file is loaded"
    let mapped = makeTextMap berzin
    -- hPutStrLn stderr "mapped dic is done"
    let value = fromMaybe "Nothing found" $ Map.lookup (TL.pack query) mapped
    if TL.null value then putStrLn "No query, no value"
    else hPutStrLn stderr $ TL.unpack value
    cli

makeTextMap :: Text -> Map Text Text
makeTextMap
    = Map.fromList
    . map ((\(y,x) -> (y, TL.drop 1 x))
    . TL.span (<'|'))
    . TL.splitOn "\n"
