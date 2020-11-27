{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sandbox where

-- import Control.Monad (when)
import System.Console.Haskeline
-- import Data.ByteString.Char8 (ByteString)
-- import System.Exit (exitSuccess)
import Data.Bifunctor (second)
import Data.Text (Text)
import Data.String
-- import Path (Abs, File, Path, filename, fromRelFile)
-- import Prettify (cyan, putTextFlush)
-- import System.IO (BufferMode (..), hPrint, hSetBuffering, hSetEcho, stderr, stdin)

-- import Handlers (Title)

import qualified Data.Text as T
-- import qualified Data.Text.IO as IO
-- import qualified Data.ByteString as BS


-- | Show duplicates and write to file.
-- getDubs :: IO ()
-- getDubs = do
--     berzin <- IO.readFile "dicts/03-Berzin"
--     putTextFlush "Berzin file is loaded"
--     let dubs = findDups berzin
--     IO.writeFile "dics/dubs" $ T.pack $ show dubs
--     hPrint stderr dubs

-- | List tuples of duplicates only from raw file.
findDups :: Text -> [(Text,Text)]
findDups
    = dups
    . map (second (T.drop 1)
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

-- | Combine answers with numbering for raw text.
-- zipWithRaw :: [ByteString] -> [Path Abs File] -> [(ByteString, Title)]
-- zipWithRaw texts files = zip texts titles
--   where
--     titles :: [Title]
--     titles = map (BC.drop 3 . BC.pack . fromRelFile . filename) files

-- | Search in raw dictionary files.
-- searchInRaw :: Text -> [(Text, Title)] -> [(Text, Title)]
-- searchInRaw query = foldl (\ acc (x,y) -> if search x == "" then acc else (search x, y) : acc) []
--   where
--     search :: Text -> Text
--     search
--         = T.unlines
--         . map (T.append (cyan "༔ ") . T.drop 1 . T.dropWhile (/= '|'))
--         . filter (T.isPrefixOf (T.append query "|"))
--         . T.lines

-- --
-- result <- runConduitRes
--     $ sourceDirectoryDeep False dir  -- [FilePath]
--     .| mapMC (\fp -> (fp,) <$> runConduit (sourceFileBS fp .| decodeUtf8C .| foldC )) -- [(FilePath,Text)]
--     .| mapC (\(f,t) -> (f, makeTextMap t)) -- [Dictionary]
--     .| mapC (\dict -> toDictionaryMeta labels' dict)
--     .| sinkList



-- Simple menu controller
-- main = do
--   hSetBuffering stdin NoBuffering
--   hSetEcho stdin False
--   key <- BS.getLine -- is not good
--   when (key /= "\ESC") $ do
--     case key of
--       "^[[A" -> putStrLn "↑"
--       "\ESC[B" -> putStrLn "↓"
--       "\ESC[C" -> putStrLn "→"
--       "\ESC[D" -> putStrLn "←"
--       "\n"     -> putStrLn "⎆"
--       "\DEL"   -> putStrLn "⎋"
--       _        -> return ()
--     main

-- main = do
--     cfg <- standardIOConfig
--     vty <- mkVty cfg
--     putStrLn "Click any button"
--     loop vty

-- loop vty = do
--     e <- nextEvent vty
--     case e of
--         EvKey KUp [] -> putStrLn "Up"
--         EvKey KDown [] -> putStrLn "Down"
--         EvKey KEsc [] -> do
--             shutdown vty
--             exitSuccess
--         _ -> putStrLn "Not Up/Down"
--     print ("Last event was: " ++ show e)
--     loop vty



main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop
