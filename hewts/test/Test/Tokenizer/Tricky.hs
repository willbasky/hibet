module Test.Tokenizer.Tricky (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

tests :: TestTree
tests =
    testGroup
        "tricky"
        [ testCase "wylie longest-match dzh stays one token" caseWylieDzh
        , testCase "wylie longest-match -d+h beats -d" caseWylieDashDH
        , testCase "wylie CRLF is a single lexical chunk" caseWylieCRLFChunk
        , testCase "wylie special marker carries InvalidSequence warning" caseWylieSpecialIssue
        , testCase "wylie longest-match k+Sh is single chunk" caseWylieKPlusShChunk
        , testCase "wylie longest-match dz+h then a" caseWylieDzPlusHThenA
        , testCase "wylie longest-match -d+h then a" caseWylieDashDPlusHThenA
        , testCase "wylie longest-match g+h then o" caseWylieGPlusHThenO
        , testCase "wylie longest-match ~M` then a" caseWylieAnusvaraThenA
        , testCase "wylie longest-match -I then a" caseWylieMinusIThenA
        , testCase "wylie longest-match ai then a" caseWylieAiThenA
        , testCase "wylie longest-match // then a" caseWylieDoubleShadThenA
        , testCase "wylie longest-match Sh then a" caseWylieShThenA
        , testCase "wylie longest-match th then a" caseWylieThThenA
        , testCase "unicode tsheg and ASCII space are different kinds" caseUnicodeTshegVsSpace
        , testCase "unicode unknown ASCII is preserved" caseUnicodeUnknownPreserved
        ]

caseWylieDzh :: Assertion
caseWylieDzh =
    case tokenizeWylie "dzh" of
        [tok] -> (tokenKind tok, tokenCanonical tok) @?= (TkConsonant, TcConsonant CdzPLUSh)
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieDashDH :: Assertion
caseWylieDashDH =
    case tokenizeWylie "-d+h" of
        [tok] -> tokenCanonical tok @?= TcConsonant CDPLUSh
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieCRLFChunk :: Assertion
caseWylieCRLFChunk =
    case tokenizeWylie "\r\n" of
        [tok] -> tokenRaw tok @?= "\r\n"
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieSpecialIssue :: Assertion
caseWylieSpecialIssue =
    case tokenizeWylie "~" of
        [tok] -> tokenIssues tok @?= [TokenIssue InvalidSequence TisWarning "Special marker out of context"]
        xs -> error $ "Expected 1 token, got " <> show (length xs)

caseWylieKPlusShChunk :: Assertion
caseWylieKPlusShChunk =
    assertWylieRawTokens "k+Sh" ["k+Sh"]

caseWylieDzPlusHThenA :: Assertion
caseWylieDzPlusHThenA =
    assertWylieRawTokens "dz+ha" ["dz+h", "a"]

caseWylieDashDPlusHThenA :: Assertion
caseWylieDashDPlusHThenA =
    assertWylieRawTokens "-d+ha" ["-d+h", "a"]

caseWylieGPlusHThenO :: Assertion
caseWylieGPlusHThenO =
    assertWylieRawTokens "g+ho" ["g+h", "o"]

caseWylieAnusvaraThenA :: Assertion
caseWylieAnusvaraThenA =
    assertWylieRawTokens "~M`a" ["~M`", "a"]

caseWylieMinusIThenA :: Assertion
caseWylieMinusIThenA =
    assertWylieRawTokens "-Ia" ["-I", "a"]

caseWylieAiThenA :: Assertion
caseWylieAiThenA =
    assertWylieRawTokens "aia" ["ai", "a"]

caseWylieDoubleShadThenA :: Assertion
caseWylieDoubleShadThenA =
    assertWylieRawTokens "//a" ["//", "a"]

caseWylieShThenA :: Assertion
caseWylieShThenA =
    assertWylieRawTokens "Sha" ["Sh", "a"]

caseWylieThThenA :: Assertion
caseWylieThThenA =
    assertWylieRawTokens "tha" ["th", "a"]

caseUnicodeTshegVsSpace :: Assertion
caseUnicodeTshegVsSpace =
    case tokenizeUnicode "་ " of
        [tshegTok, spaceTok] -> do
            tokenKind tshegTok @?= TkPunctuation
            tokenKind spaceTok @?= TkSpace
        xs -> error $ "Expected 2 tokens, got " <> show (length xs)

caseUnicodeUnknownPreserved :: Assertion
caseUnicodeUnknownPreserved =
    case tokenizeUnicode "x" of
        [tok] -> do
            tokenCanonical tok @?= TcUnknown (UnknownMark "x")
            tokenIssues tok @?= [TokenIssue UnknownChar TisWarning "Unknown token"]
        xs -> error $ "Expected 1 token, got " <> show (length xs)

assertWylieRawTokens :: Text -> [Text] -> Assertion
assertWylieRawTokens input expected =
    (tokenRaw <$> tokenizeWylie input) @?= expected
