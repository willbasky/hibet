module Main (main) where

import qualified Test.Convert as Convert
import qualified Test.Convert.Golden as ConvertGolden
import qualified Test.Convert.Sentence as Sentence
import qualified Test.Grammar.Constraint.Constraint01 as Constraint01
import qualified Test.Grammar.Constraint.Constraint08 as Constraint08
import qualified Test.Grammar.Constraint.Constraint09 as Constraint09
import qualified Test.Grammar.Constraint.Constraint10 as Constraint10
import qualified Test.Grammar.Constraint.Constraint11 as Constraint11
import qualified Test.Grammar.Constraint.Constraint12 as Constraint12
import qualified Test.Grammar.Constraint.Constraint13 as Constraint13
import qualified Test.Grammar.Constraint.Constraint14 as Constraint14
import qualified Test.Grammar.Constraint.Constraint15 as Constraint15
import qualified Test.Grammar.Constraint.Constraint16 as Constraint16
import qualified Test.Grammar.Constraint.Constraint17 as Constraint17
import qualified Test.Grammar.Constraint.Constraint18 as Constraint18
import qualified Test.Grammar.Constraint.Constraint19 as Constraint19
import qualified Test.Grammar.Constraint.Constraint20 as Constraint20
import qualified Test.Grammar.Stack as Stack
import qualified Test.Grammar.Structure as Structures
import qualified Test.Parity as Parity
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tokenizer.Golden as Golden
import qualified Test.Tokenizer.Integration as Integration
import qualified Test.Tokenizer.RoundTrip as RoundTrip
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
        , RoundTrip.tests
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
        , Constraint19.tests
        , Constraint20.tests
        , Stack.tests
        , Structures.tests
        , Sentence.tests
        , Convert.tests
        , ConvertGolden.tests
        , Parity.tests
        ]
