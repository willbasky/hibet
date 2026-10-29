-- Ожидания: спецификация (`docs/SPEC.md`, S1–S3, S5, S6, S8) и корпуса
-- эталонов (ewts-rs) для сквозного прогона через дверь API.
module Test.Api (tests) where

import Convert
    ( InputFormat (..)
    , Settings (..)
    , checkSpelling
    , convertUnicodeToWylie
    , convertWylieToUnicode
    , defaultSettings
    , splitSentencesWith
    )
import Convert.Diagnostic
    ( Diagnostic (..)
    , DiagnosticCode (..)
    , Diagnostics
    , diagnosticList
    )
import Test.Corpus (Row (..), readRows)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "public api"
        [ testCase "Wylie in, script out (S1)" $ do
            fmap fst (convertWylieToUnicode defaultSettings "bkra shis")
                @?= Right "བཀྲ་ཤིས"
        , testCase "script in, Wylie out (S2)" $ do
            fmap fst (convertUnicodeToWylie defaultSettings "བཀྲ་ཤིས")
                @?= Right "bkra shis"
        , testCase "the check returns findings and no text (S3, S5)" $ do
            fmap codes (checkSpelling (defaultSettings{inputFormat = InWylie}) "bk")
                @?= Right [MissingVowelAfterPrefix]
        , testCase "the check finds what the converter reports" $ do
            let wylie = defaultSettings{inputFormat = InWylie}
            fmap codes (checkSpelling wylie "bk")
                @?= fmap (codes . snd) (convertWylieToUnicode wylie "bk")
        , testCase "checks off: the same text, no records (S6)" $ do
            let off = defaultSettings{inputFormat = InWylie, withChecks = False}
                on = defaultSettings{inputFormat = InWylie}
            fmap fst (convertWylieToUnicode off "bk")
                @?= fmap fst (convertWylieToUnicode on "bk")
            fmap (codes . snd) (convertWylieToUnicode off "bk")
                @?= Right []
        , testCase "the document warning is the converter's, not the parse's (O6)" $ do
            -- The parse contract says nothing about the document as a whole;
            -- the converter adds the warning itself.
            fmap (codes . snd) (splitSentencesWith "abc") @?= Right []
            fmap (codes . snd) (convertUnicodeToWylie defaultSettings "abc")
                @?= Right [NoTibetanCharacters]
        , testCase "the reference corpus runs through the door" checkCorpus
        ]

-- | The codes of the findings, in order.
codes :: Diagnostics -> [DiagnosticCode]
codes = map diagCode . diagnosticList

-- | The whole ewts-rs corpus (16 rules, no accepted divergences) through the
-- public W→U entry point: the door has to hand the same text the internals do.
checkCorpus :: Assertion
checkCorpus = do
    rows <- readRows "test/vectors/ewts_rs_rules.tsv"
    let bad =
            [ (rowLine r, rowWylie r, got)
            | r <- rows
            , let got = fmap fst (convertWylieToUnicode defaultSettings (rowWylie r))
            , got /= Right (rowUnicode r)
            ]
    assertBool
        ("the api disagrees with the corpus:\n" <> unlines [show b | b <- bad])
        (null bad)
