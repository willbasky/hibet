-- Ожидания: спецификация (`docs/SPEC.md`, S1–S3, S5–S8) и корпуса
-- эталонов (ewts-rs, java sloppy) для сквозного прогона через дверь API.
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
import Convert.Tokenizer.Wylie (normalizeSloppy)
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
        , testCase "vocalic r-i reads the root, not a subjoined ra (D4)" $
            fmap fst (convertWylieToUnicode defaultSettings "r-i")
                @?= Right "རྀ"
        , testCase "vocalic r-I keeps the long sign (D4)" $
            fmap fst (convertWylieToUnicode defaultSettings "r-I")
                @?= Right "རཱྀ"
        , testCase "vocalic kr-i stays subjoined (D4)" $
            fmap fst (convertWylieToUnicode defaultSettings "kr-i")
                @?= Right "ཀྲྀ"
        , testCase "a vowel after a final opens a new syllable (D4)" $
            fmap fst (convertWylieToUnicode defaultSettings "rM-i")
                @?= Right "རཾཨྀ"
        , testCase "a-chen policy: Wylie ae is ཨཨེ, e is ཨེ (Q6/Q7)" $ do
            fmap fst (convertWylieToUnicode defaultSettings "ae") @?= Right "ཨཨེ"
            fmap fst (convertWylieToUnicode defaultSettings "e") @?= Right "ཨེ"
        , testCase "a-chen policy: ཨཨེ is ae, ཨེ is e (Q6/Q7)" $ do
            fmap fst (convertUnicodeToWylie defaultSettings "ཨཨེ") @?= Right "ae"
            fmap fst (convertUnicodeToWylie defaultSettings "ཨེ") @?= Right "e"
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
        , testCase "sloppy: the java vector converts (S7)" checkSloppyVector
        , testCase "sloppy: the reference's own assertion (S7)" $
            normalizeSloppy "Mi la lHan Mi kaHthog 'uM Ma m "
                @?= "mi la lhan mi kaHthog 'uM ma ma "
        , testCase "sloppy: a lone b and m take their vowel (S7)" $
            fmap fst (convertWylieToUnicode sloppySettings " b m ") @?= Right "བ་མ་"
        , testCase "sloppy: S lowers, Sh stays (S7)" $
            normalizeSloppy "S Sh" @?= "s Sh"
        , testCase "sloppy off: the vector is read as written (S7)" $ do
            row : _ <- readRows "test/vectors/java_sloppy.tsv"
            let got = fmap fst (convertWylieToUnicode wylieStrict (rowWylie row))
            assertBool
                ("the strict door corrected the input: " <> show got)
                (got /= Right (rowUnicode row))
        , testCase "strict: the preferred spelling is reported only when asked (S8)" $ do
            fmap codes (checkSpelling wylieStrict "dga") @?= Right []
            fmap codes (checkSpelling strictChecks "dga") @?= Right [AmbiguousSpelling]
        , testCase
            "strict: the rare superfix combination is reported only when asked (S8)"
            $ do
                fmap codes (checkSpelling wylieStrict "rkwa") @?= Right []
                fmap codes (checkSpelling strictChecks "rkwa")
                    @?= Right [IllegalSuperfixCombination]
        , testCase "strict: a real defect is reported in both modes (S8)" $ do
            fmap codes (checkSpelling wylieStrict "tgra") @?= Right [InvalidPrefix]
            fmap codes (checkSpelling strictChecks "tgra") @?= Right [InvalidPrefix]
        ]

-- | The codes of the findings, in order.
codes :: Diagnostics -> [DiagnosticCode]
codes = map diagCode . diagnosticList

-- | Wylie with the input corrections on ("with corrections", S7).
sloppySettings :: Settings
sloppySettings = defaultSettings{inputFormat = InWylie, sloppy = True}

-- | Wylie read as written: no corrections.
wylieStrict :: Settings
wylieStrict = defaultSettings{inputFormat = InWylie}

-- | Wylie with the strict spelling check on: the normal findings plus the
-- strict-only ones.
strictChecks :: Settings
strictChecks = wylieStrict{strict = True}

-- | The whole sloppy vector: the reference converts these only through its
-- sloppy path, so the flag has to be on for the door to match.
checkSloppyVector :: Assertion
checkSloppyVector = do
    rows <- readRows "test/vectors/java_sloppy.tsv"
    let bad =
            [ (rowLine r, got)
            | r <- rows
            , let got = fmap fst (convertWylieToUnicode sloppySettings (rowWylie r))
            , got /= Right (rowUnicode r)
            ]
    assertBool
        ("the sloppy api disagrees with the vector:\n" <> unlines [show b | b <- bad])
        (null bad)

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
