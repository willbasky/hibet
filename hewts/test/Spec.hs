module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tokenizer.Golden as Golden
import qualified Test.Tokenizer.Integration as Integration
import qualified Test.Tokenizer.Spans as Spans
import qualified Test.Tokenizer.Tricky as Tricky
import qualified Test.Tokenizer.Types as Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "hewts tokenizers"
        [ Types.tests
        , Spans.tests
        , Tricky.tests
        , Integration.tests
        , Golden.tests
        ]
