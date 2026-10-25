{-# OPTIONS_GHC -fno-full-laziness #-}

-- | REFACTOR-AST §9.1 item 9: the parse stays linear. A timing test in the
-- suite can only afford small inputs, so the corpus-sized run lives in
-- @benchmark/Main.hs@; this one keeps the regression from coming back silently.
module Test.Performance (tests) where

import Control.Exception (evaluate)
import Convert (SpellItem, splitSentences)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Test.Corpus (kangyurText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Text.Printf (printf)

tests :: TestTree
tests =
    testGroup
        "performance"
        [ testCase "doubling the input grows the time at most 2.5x" checkDoubling
        ]

checkDoubling :: Assertion
checkDoubling = do
    kang <- kangyurText
    let small = T.take 2000 kang
        large = T.take 4000 kang
    smallTime <- fastest 7 small
    largeTime <- fastest 7 large
    let ratio = largeTime / smallTime
    assertBool
        ( printf
            "doubling 2000 -> 4000 characters took %.2fx (limit 2.5x: %.3f s -> %.3f s)"
            ratio
            smallTime
            largeTime
        )
        (ratio <= 2.5)

-- | The best of @n@ runs: the fastest run is the one least disturbed by the
-- rest of the machine.
fastest :: Int -> T.Text -> IO Double
fastest n input = minimum <$> sequence [measure input | _ <- [1 .. n]]

measure :: T.Text -> IO Double
measure input = do
    start <- getMonotonicTimeNSec
    forced <- evaluate (force (splitSentences input))
    forced `seq` pure ()
    end <- getMonotonicTimeNSec
    pure (fromIntegral (end - start) / 1e9)

-- | The whole result, not just its first constructor: the parse is lazy, so a
-- shallow look would measure almost nothing.
force :: Either T.Text [SpellItem] -> Int
force (Left err) = T.length err
force (Right items) = length (show items)
