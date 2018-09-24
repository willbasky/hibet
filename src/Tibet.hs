{-# LANGUAGE TemplateHaskell #-}

module Tibet
       ( start
       ) where

import           Handlers (makeTextMap, mapMaybeTuple)

import           Data.Map (Map)
import           Data.Text (Text)
import           Path (File, Path, filename, fromAbsFile, fromRelFile, mkRelDir)
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
    let zipped = zipWithTitles texts files
    let dscValues = mapMaybeTuple (Map.lookup query) zipped
    if null dscValues then putStrLn "Nothing found"
    else IO.hPutStrLn stderr $ mergeWithTitles dscValues
    cli

mergeWithTitles :: [(Text, Text)] -> Text
mergeWithTitles dscValues = T.unlines $ zipWith flatten numbers ascValues
  where
    ascValues :: [(Text, Text)]
    ascValues = map (\(v,t) -> (T.unlines . reverse . T.lines $ v, t)) dscValues

    numbers :: [Text]
    numbers = map ((\x -> T.append (T.pack x) ". ") . show) [1::Int ..]

    flatten :: Text -> (Text, Text) -> Text
    flatten n (v, t) = T.append (T.append n (T.append t "\n")) v

zipWithTitles :: [Text] -> [Path b File] -> [(Map Text Text, Text)]
zipWithTitles texts files = zip mapped titles
  where
    mapped :: [Map Text Text]
    mapped = map makeTextMap texts

    titles :: [Text]
    titles = map (T.drop 3 . T.pack . fromRelFile . filename) files
