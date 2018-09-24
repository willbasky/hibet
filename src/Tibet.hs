{-# LANGUAGE TemplateHaskell #-}

module Tibet
       ( start
       ) where

import           Handlers (makeTextMap, mapMaybeTuple)

import           Data.Map (Map)
import           Data.Text.Lazy (Text)
import           Path (File, Path, filename, fromAbsFile, fromRelFile, mkRelDir)
import           Path.IO (listDir)
import           System.IO (stderr)

import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as LIO


start :: IO ()
start = do
    LIO.hPutStrLn stderr "What tibetan word to translate?"
    cli

cli :: IO ()
cli = do
    query <- LIO.hPutStr stderr "> " >> LIO.getLine
    (_, files) <- listDir $(mkRelDir "./dics/")
    texts <- mapM (LIO.readFile . fromAbsFile) files
    let zipped = zipWithTitles texts files
    let dscValues = mapMaybeTuple (Map.lookup query) zipped
    if null dscValues then putStrLn "Nothing found"
    else LIO.hPutStrLn stderr $ mergeWithTitles dscValues
    cli

mergeWithTitles :: [(Text, Text)] -> Text
mergeWithTitles dscValues = TL.unlines $ zipWith flatten numbers ascValues
  where
    ascValues :: [(Text, Text)]
    ascValues = map (\(v,t) -> (TL.unlines . reverse . TL.lines $ v, t)) dscValues

    numbers :: [Text]
    numbers = map ((\x -> TL.append (TL.pack x) ". ") . show) [1::Int ..]

    flatten :: Text -> (Text, Text) -> Text
    flatten n (v, t) = TL.append (TL.append n (TL.append t "\n")) v

zipWithTitles :: [Text] -> [Path b File] -> [(Map Text Text, Text)]
zipWithTitles texts files = zip mapped titles
  where
    mapped :: [Map Text Text]
    mapped = map makeTextMap texts

    titles :: [Text]
    titles = map (TL.drop 3 . TL.pack . fromRelFile . filename) files
