{-# OPTIONS_GHC -fno-full-laziness #-}

-- | REFACTOR-AST §9.1 item 9: the parse stays linear. Doubling the input may
-- grow the time at most 2.5 times; the run prints the numbers and fails on a
-- superlinear step. The suite keeps a small-input version (Test.Performance),
-- so a regression shows up in @cabal test@ as well.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Convert (SpellItem, splitSentences)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (exitFailure)
import Text.Printf (printf)

-- | Prefix sizes of the Kangyur text; each is the double of the one before, so
-- the step shows the growth of the parse itself.
sizes :: [Int]
sizes = [10000, 20000, 40000]

main :: IO ()
main = do
    kang <- readUtf8File "test/vectors/kang.txt"
    let text = T.dropWhile (== '\xfeff') kang
    results <- forM sizes $ \n -> do
        let chunk = T.take n text
        seconds <- fast 3 chunk
        printf "%6d characters  %6.3f s\n" (T.length chunk) seconds
        pure seconds
    let steps = zipWith (/) (drop 1 results) results
    mapM_
        (\(n, ratio) -> printf "doubling to %d characters  %.2fx\n" n ratio)
        (zip (drop 1 sizes) steps)
    unless (all (<= 2.5) steps) $ do
        putStrLn "superlinear: doubling the input grew the time over 2.5x"
        exitFailure

-- | The best of @n@ runs: the fastest run is the one least disturbed by the
-- rest of the machine.
fast :: Int -> Text -> IO Double
fast n input = minimum <$> sequence [measure input | _ <- [1 .. n]]

measure :: Text -> IO Double
measure input = do
    start <- getMonotonicTimeNSec
    forced <- evaluate (force (splitSentences input))
    forced `seq` pure ()
    end <- getMonotonicTimeNSec
    pure (fromIntegral (end - start) / 1e9)

-- | The whole result, not just its first constructor: the parse is lazy, so a
-- shallow look would measure almost nothing.
force :: Either Text [SpellItem] -> Int
force (Left err) = T.length err
force (Right items) = length (show items)

-- | Read the corpus as UTF-8 regardless of the locale, as the suite does.
readUtf8File :: FilePath -> IO Text
readUtf8File path = do
    bytes <- BS.readFile path
    case TE.decodeUtf8' bytes of
        Left err -> error (path ++ ": not valid UTF-8: " <> show err)
        Right text -> pure text
