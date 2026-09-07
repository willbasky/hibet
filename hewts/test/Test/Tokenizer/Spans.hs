module Test.Tokenizer.Spans (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

tests :: TestTree
tests =
    testGroup
        "spans"
        [ testCase "wylie spans are contiguous" (assertContiguousSpans $ tokenizeWylie "tsh + //")
        , testCase "wylie multi-char token span length" caseWylieMultiCharLen
        , testCase "unicode spans are contiguous" (assertContiguousSpans $ tokenizeUnicode "ཚ །།")
        , testCase "unicode final token ends at input length" caseUnicodeEndsAtLength
        ]

caseWylieMultiCharLen :: Assertion
caseWylieMultiCharLen =
    case tokenizeWylie "g+h" of
        [tok] -> tokenSpan tok @?= mkSpan 0 3
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseUnicodeEndsAtLength :: Assertion
caseUnicodeEndsAtLength =
    let input = "གཞོན"
        toks = tokenizeUnicode input
        expectedEnd = fromIntegral (T.length input)
     in case reverse toks of
            [] -> error "Expected non-empty token stream"
            tok : _ -> offsetEnd (tokenSpan tok) @?= expectedEnd

assertContiguousSpans :: [Token] -> Assertion
assertContiguousSpans [] = pure ()
assertContiguousSpans toks = go 0 toks
  where
    go :: Natural -> [Token] -> Assertion
    go _ [] = pure ()
    go expectedStart (tok : rest) = do
        offsetStart (tokenSpan tok) @?= expectedStart
        let end = offsetEnd (tokenSpan tok)
        end @?= expectedStart + fromIntegral (T.length (tokenRaw tok))
        go end rest
