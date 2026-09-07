module Test.Tokenizer.Integration (tests) where

import Convert.Token
import Convert.Tokenizer.Unicode (tokenizeUnicode)
import Convert.Tokenizer.Wylie (tokenizeWylie)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, testCase)

tests :: TestTree
tests =
    testGroup
        "integration"
        [ testCase "wylie raw roundtrip through tokens" caseWylieRawRoundtrip
        , testCase "unicode raw roundtrip through tokens" caseUnicodeRawRoundtrip
        , testCase "wylie aliases normalize to canonical rendering" caseWylieCanonicalRender
        , testCase "unicode aliases normalize to canonical rendering" caseUnicodeCanonicalRender
        ]

caseWylieRawRoundtrip :: Assertion
caseWylieRawRoundtrip =
    let input = "gzhon nu'i dpe cha //"
     in rawRoundtripWylie input @?= input

caseUnicodeRawRoundtrip :: Assertion
caseUnicodeRawRoundtrip =
    let input = "གཞོན་ནུའི་དཔེ་ཆ།།"
     in rawRoundtripUnicode input @?= input

caseWylieCanonicalRender :: Assertion
caseWylieCanonicalRender =
    canonicalWylieFromWylie "W O ~M`" @?= "w o M"

caseUnicodeCanonicalRender :: Assertion
caseUnicodeCanonicalRender =
    canonicalWylieFromUnicode "ཝ ོ ཾ" @?= "w o M"

rawRoundtripWylie :: Text -> Text
rawRoundtripWylie = T.concat . fmap tokenRaw . tokenizeWylie

rawRoundtripUnicode :: Text -> Text
rawRoundtripUnicode = T.concat . fmap tokenRaw . tokenizeUnicode

canonicalWylieFromWylie :: Text -> Text
canonicalWylieFromWylie = T.concat . fmap canonicalPiece . tokenizeWylie

canonicalWylieFromUnicode :: Text -> Text
canonicalWylieFromUnicode = T.concat . fmap canonicalPiece . tokenizeUnicode

canonicalPiece :: Token -> Text
canonicalPiece tok =
    case tokenCanonical tok of
        TcConsonant Cw -> "w"
        TcConsonant CgPLUSh -> "g+h"
        TcVowel Vo -> "o"
        TcFinal FMAnusvara -> "M"
        TcPunctuation PMTsheg -> " "
        TcSpace SMSpace -> " "
        TcUnknown (UnknownMark raw) -> raw
        _ -> tokenRaw tok
