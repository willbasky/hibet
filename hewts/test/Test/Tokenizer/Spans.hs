module Test.Tokenizer.Spans (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "spans"
        [ testCase "wylie spans are contiguous" (assertContiguousSpans $ tokenizeWylie "tsh + //")
        , testCase "wylie multi-char token span length" caseWylieMultiCharLen
        , testCase "unicode spans are contiguous" (assertContiguousSpans $ tokenizeUnicode "ཚ །།")
        , testCase "unicode final token ends at input length" caseUnicodeEndsAtLength
        , testCase "wylie stream invariants on mixed input" caseWylieStreamInvariants
        , testCase "unicode stream invariants on mixed input" caseUnicodeStreamInvariants
        , testCase "wylie stream invariants on empty input" (assertTokenStreamInvariants tokenizeWylie "")
        , testCase "unicode stream invariants on empty input" (assertTokenStreamInvariants tokenizeUnicode "")
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

caseWylieStreamInvariants :: Assertion
caseWylieStreamInvariants =
    assertTokenStreamInvariants tokenizeWylie "gzhon // ~+`]-. x _ k+Sh"

caseUnicodeStreamInvariants :: Assertion
caseUnicodeStreamInvariants =
    assertTokenStreamInvariants tokenizeUnicode "ཀིི ཀxི ྆། ་"

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

assertTokenStreamInvariants :: (T.Text -> [Token]) -> T.Text -> Assertion
assertTokenStreamInvariants tokenizer input = do
    let toks = tokenizer input
    assertBool "Token stream contains empty tokenRaw" (all (not . T.null . tokenRaw) toks)
    T.concat (map tokenRaw toks) @?= input
    mapM_ assertUnknownCanonicalEqualsRaw toks

assertUnknownCanonicalEqualsRaw :: Token -> Assertion
assertUnknownCanonicalEqualsRaw tok =
    case tokenKind tok of
        TkUnknown -> tokenCanonical tok @?= TcUnknown (UnknownMark (tokenRaw tok))
        _ -> pure ()
