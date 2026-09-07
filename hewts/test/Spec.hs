module Main (main) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "hewts tokenizers"
        [ testGroup "edge" edgeTests
        , testGroup "golden" goldenTests
        ]

edgeTests :: [TestTree]
edgeTests =
    [ testCase "wylie longest-match keeps tsh as single token" caseWylieLongestMatch
    , testCase "wylie special marker out of context emits warning" caseWylieSpecialWarning
    , testCase "unicode classifies Tibetan punctuation" caseUnicodePunctuation
    , testCase "spans are contiguous for wylie sample" caseWylieSpansContiguous
    , testCase "spans are contiguous for unicode sample" caseUnicodeSpansContiguous
    ]

goldenTests :: [TestTree]
goldenTests =
    [ goldenVsString
        "wylie token stream"
        "test/golden/wylie_tokens.golden"
        (pure $ renderTokensBS $ tokenizeWylie "gzhon nu'i dpe cha // +")
    , goldenVsString
        "unicode token stream"
        "test/golden/unicode_tokens.golden"
        (pure $ renderTokensBS $ tokenizeUnicode "གཞོན་ནུའི་དཔེ་ཆ།། ་x")
    ]

caseWylieLongestMatch :: Assertion
caseWylieLongestMatch =
    tokenCanonical <$> tokenizeWylie "tsh" @?= [TcConsonant Ctsh]

caseWylieSpecialWarning :: Assertion
caseWylieSpecialWarning =
    case tokenizeWylie "+" of
        [tok] -> do
            tokenKind tok @?= TkUnknown
            tokenIssues tok @?= [TokenIssue InvalidSequence TisWarning "Special marker out of context"]
        xs -> error $ "Expected single token, got " <> show (length xs)

caseUnicodePunctuation :: Assertion
caseUnicodePunctuation =
    tokenCanonical <$> tokenizeUnicode "།" @?= [TcPunctuation PMShad]

caseWylieSpansContiguous :: Assertion
caseWylieSpansContiguous =
    assertContiguousSpans (tokenizeWylie "tsh + //")

caseUnicodeSpansContiguous :: Assertion
caseUnicodeSpansContiguous =
    assertContiguousSpans (tokenizeUnicode "ཚ །།")

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

renderTokensBS :: [Token] -> ByteString
renderTokensBS = BL.fromStrict . TE.encodeUtf8 . T.unlines . fmap renderToken

renderToken :: Token -> Text
renderToken tok =
    T.pack (show (tokenSource tok))
        <> "|"
        <> T.pack (show (tokenKind tok))
        <> "|"
        <> T.pack (show (tokenCanonical tok))
        <> "|raw="
        <> tokenRaw tok
        <> "|span="
        <> spanText (tokenSpan tok)
        <> "|issues="
        <> issuesText (tokenIssues tok)

spanText :: Span -> Text
spanText span =
    T.pack (show (offsetStart span)) <> ":" <> T.pack (show (offsetEnd span))

issuesText :: [TokenIssue] -> Text
issuesText [] = "[]"
issuesText xs = T.pack (show xs)
