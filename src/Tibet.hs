{-# LANGUAGE TemplateHaskell #-}

module Tibet
       ( start
       ) where

import           Handlers (makeTextMap)

import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Path (fromAbsFile, mkRelDir)
import           Path.IO (listDir)
import           System.IO (stderr)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO


start :: IO ()
start = do
    IO.hPutStrLn stderr "What tibetan word to translate?"
    cli

cli :: IO ()
cli = do
    query <- IO.hPutStr stderr "> " >> IO.getLine
    (_, files) <- listDir $(mkRelDir "./dics/")
    texts <- mapM (IO.readFile . fromAbsFile) files
    let mapped = map makeTextMap texts
    let valueDsc = mapMaybe (Map.lookup query) mapped
    if null valueDsc then putStrLn "Nothing found"
    else IO.hPutStrLn stderr $ valueAsc valueDsc
    cli

valueAsc :: [Text] -> Text
valueAsc valueDsc
    = T.unlines
    $ zipWith T.append
        (map ((\x -> T.append (T.pack x) ".\n") . show) [1::Int ..])
        (map (T.unlines . reverse . T.lines) valueDsc)

