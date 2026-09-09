module Test.Tokenizer.Integration (tests) where

import Convert (OutputFormat (..), SpellItem (..), renderItems, splitSentences)
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
        , testCase "wylie f and v keep canonical distinction" caseWylieFvCanonicalDistinct
        , testCase "unicode aliases normalize to canonical rendering" caseUnicodeCanonicalRender
        , testCase "render to wylie from unicode" caseRenderUnicodeToWylie
        , testCase "render to unicode from wylie" caseRenderWylieToUnicode
        , testCase "same-script render keeps everything raw" caseRenderSameScriptKeepsRaw
        , testCase "cross-script render f and v compose" caseRenderFvComposed
        , testCase "cross-script render normalizes to canonical" caseRenderCrossScriptCanonical
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

caseWylieFvCanonicalDistinct :: Assertion
caseWylieFvCanonicalDistinct =
    canonicalWylieFromWylie "f v ph b" @?= "f v ph b"

caseUnicodeCanonicalRender :: Assertion
caseUnicodeCanonicalRender =
    canonicalWylieFromUnicode "ཝ ོ ཾ" @?= "w o M"

caseRenderUnicodeToWylie :: Assertion
caseRenderUnicodeToWylie =
    renderInput OutWylie "ཀི་" @?= "ki "

caseRenderWylieToUnicode :: Assertion
caseRenderWylieToUnicode =
    renderFromTokens OutUnicode (tokenizeWylie "ki ") @?= "ཀི་"

caseRenderSameScriptKeepsRaw :: Assertion
caseRenderSameScriptKeepsRaw =
    renderInput OutUnicode "ཀི་" @?= "ཀི་"

caseRenderFvComposed :: Assertion
caseRenderFvComposed =
    renderFromTokens OutUnicode (tokenizeWylie "f v ") @?= "ཕ༹་བ༹་"

caseRenderCrossScriptCanonical :: Assertion
caseRenderCrossScriptCanonical =
    renderInput OutWylie "ཉ་ཱི" @?= "ny Ai"

rawRoundtripWylie :: Text -> Text
rawRoundtripWylie = T.concat . fmap tokenRaw . tokenizeWylie

rawRoundtripUnicode :: Text -> Text
rawRoundtripUnicode = T.concat . fmap tokenRaw . tokenizeUnicode

renderInput :: OutputFormat -> Text -> Text
renderInput fmt = either (error . T.unpack) (renderItems fmt) . splitSentences

renderFromTokens :: OutputFormat -> [Token] -> Text
renderFromTokens fmt = renderItems fmt . map (Other . (: []))

canonicalWylieFromWylie :: Text -> Text
canonicalWylieFromWylie = T.concat . fmap canonicalPiece . tokenizeWylie

canonicalWylieFromUnicode :: Text -> Text
canonicalWylieFromUnicode = T.concat . fmap canonicalPiece . tokenizeUnicode

canonicalPiece :: Token -> Text
canonicalPiece tok =
    case tokenCanonical tok of
        TcConsonant Cw -> "w"
        TcConsonant CgPLUSh -> "g+h"
        TcConsonant Cf -> "f"
        TcConsonant Cv -> "v"
        TcVowel Vo -> "o"
        TcFinal FMAnusvara -> "M"
        TcPunctuation PMTsheg -> " "
        TcSpace SMSpace -> " "
        TcUnknown (UnknownMark raw) -> raw
        _ -> tokenRaw tok
