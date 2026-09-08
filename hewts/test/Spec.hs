module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Grammar.Rule.Constraint01 as Constraint01
import qualified Test.Grammar.Rule.Constraint08 as Constraint08
import qualified Test.Grammar.Rule.Constraint09 as Constraint09
import qualified Test.Grammar.Rule.Constraint10 as Constraint10
import qualified Test.Grammar.Rule.Constraint11 as Constraint11
import qualified Test.Grammar.Rule.Constraint12 as Constraint12
import qualified Test.Grammar.Rule.Constraint13 as Constraint13
import qualified Test.Grammar.Rule.Constraint14 as Constraint14
import qualified Test.Grammar.Rule.Constraint15 as Constraint15
import qualified Test.Grammar.Rule.Constraint16 as Constraint16
import qualified Test.Grammar.Rule.Constraint17 as Constraint17
import qualified Test.Grammar.Rule.Constraint18 as Constraint18
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
        , Constraint01.tests
        , Constraint08.tests
        , Constraint09.tests
        , Constraint10.tests
        , Constraint11.tests
        , Constraint12.tests
        , Constraint13.tests
        , Constraint14.tests
        , Constraint15.tests
        , Constraint16.tests
        , Constraint17.tests
        , Constraint18.tests
        ]
