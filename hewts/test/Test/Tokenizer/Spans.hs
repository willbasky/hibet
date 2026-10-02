module Test.Tokenizer.Spans (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "spans"
        [ testCase
            "wylie spans are contiguous"
            (assertContiguousSpans $ fst (tokenizeWylie "tsh // g+ha + //"))
        , testCase "wylie multi-char token span length" caseWylieMultiCharLen
        , testCase
            "unicode spans are contiguous"
            (assertContiguousSpans $ fst (tokenizeUnicode "ཚ དྷ།།"))
        , testCase "unicode final token ends at input length" caseUnicodeEndsAtLength
        , testCase "wylie stream invariants on mixed input" caseWylieStreamInvariants
        , testCase "unicode stream invariants on mixed input" caseUnicodeStreamInvariants
        , testCase
            "wylie stream invariants on empty input"
            (assertTokenStreamInvariants (fst . tokenizeWylie) "")
        , testCase
            "unicode stream invariants on empty input"
            (assertTokenStreamInvariants (fst . tokenizeUnicode) "")
        ]

caseWylieMultiCharLen :: Assertion
caseWylieMultiCharLen =
    case fst (tokenizeWylie "g+h") of
        [tok, cont] -> do
            tokenSpan tok @?= mkSpan 0 3
            tokenRaw cont @?= ""
            tokenSpan cont @?= mkSpan 3 3
            tokenCanonical cont @?= TcSubConsonant SCh
        xs -> error $ "Expected 2 tokens, got " <> show (length xs)

caseUnicodeEndsAtLength :: Assertion
caseUnicodeEndsAtLength =
    let input = "གཞོན"
        toks = fst (tokenizeUnicode input)
        expectedEnd = fromIntegral (T.length input)
     in case reverse toks of
            [] -> error "Expected non-empty token stream"
            tok : _ -> offsetEnd (tokenSpan tok) @?= expectedEnd

caseWylieStreamInvariants :: Assertion
caseWylieStreamInvariants =
    assertTokenStreamInvariants
        (fst . tokenizeWylie)
        "gzhon // g+h O ~+`]-. x _ k+Sh"

caseUnicodeStreamInvariants :: Assertion
caseUnicodeStreamInvariants =
    assertTokenStreamInvariants (fst . tokenizeUnicode) "ཀིི ཀདྷ ཀxི ྆། ་"

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
    mapM_ assertContinuationShape toks
    T.concat (map tokenRaw toks) @?= input
    mapM_ assertUnknownCanonicalEqualsRaw toks

-- A token carries an empty raw slice exactly when it is a continuation token
-- of a decomposed spelling: its span is degenerate (start == end). Ordinary
-- tokens always carry a non-empty raw slice.
assertContinuationShape :: Token -> Assertion
assertContinuationShape tok =
    let rawEmpty = T.null (tokenRaw tok)
        spanEmpty = offsetStart (tokenSpan tok) == offsetEnd (tokenSpan tok)
     in assertBool
            "empty tokenRaw must coincide with a degenerate span"
            (rawEmpty == spanEmpty)

assertUnknownCanonicalEqualsRaw :: Token -> Assertion
assertUnknownCanonicalEqualsRaw tok =
    case tokenKind tok of
        TkUnknown -> tokenCanonical tok @?= TcUnknown (UnknownMark (tokenRaw tok))
        _ -> pure ()
