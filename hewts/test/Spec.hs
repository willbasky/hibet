module Main (main) where

import qualified Test.Api as Api
import qualified Test.Architecture as Architecture
import qualified Test.Convert as Convert
import qualified Test.Convert.Golden as ConvertGolden
import qualified Test.Diagnostic as Diagnostic
import qualified Test.Fuzz as Fuzz
import qualified Test.Fuzz.Structure as FuzzStructure
import qualified Test.Grammar.Book as Book
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
import qualified Test.Grammar.Sentence as Sentence
import qualified Test.Grammar.Stack as Stack
import qualified Test.Grammar.Structure as Structures
import qualified Test.Grammar.Syllable as Syllable
import qualified Test.Grammar.Wylie as Wylie
import qualified Test.Integration as Integration
import qualified Test.Parity as Parity
import qualified Test.Performance as Performance
import qualified Test.Render.Wylie as WylieOutput
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tokenizer.Classification as Classification
import qualified Test.Tokenizer.Golden as Golden
import qualified Test.Tokenizer.RoundTrip as RoundTrip
import qualified Test.Tokenizer.Spans as Spans
import qualified Test.Tokenizer.Tricky as Tricky

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "hewts"
        [ Architecture.tests
        , Classification.tests
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
        , Book.tests
        , Diagnostic.tests
        , Fuzz.tests
        , FuzzStructure.tests
        , Stack.tests
        , Structures.tests
        , Syllable.tests
        , Wylie.tests
        , WylieOutput.tests
        , Api.tests
        , Sentence.tests
        , Convert.tests
        , ConvertGolden.tests
        , Performance.tests
        , Parity.tests
        ]
