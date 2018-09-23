module Tibet
       ( start
       ) where

import           Handlers (makeTextMap)

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           System.IO (hPutStr, hPutStrLn, stderr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO


start :: IO ()
start = do
    hPutStrLn stderr "What tibetan word to translate?"
    cli

cli :: IO ()
cli = do
    query <- hPutStr stderr "> " >> getLine
    berzin <- IO.readFile "dics/03-Berzin"
    let mapped = makeTextMap berzin
    let valueDsc = fromMaybe "Nothing found" $ Map.lookup (T.pack query) mapped
    if valueDsc == "Nothing found" then putStrLn "Nothing found"
    else IO.hPutStrLn stderr $ valueAsc valueDsc
    cli

valueAsc :: Text -> Text
valueAsc valueDsc
    = T.unlines
    $ zipWith T.append
        (map ((\x -> T.append (T.pack x) ". ") . show) [1::Int ..])
        (reverse . T.lines $ valueDsc)
