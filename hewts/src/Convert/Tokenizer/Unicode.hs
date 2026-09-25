module Convert.Tokenizer.Unicode where

import Convert.Token
import Control.Applicative (asum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Tokenize Unicode Tibetan input to typed IR tokens. Deprecated
-- precomposed characters (aspirates, long vowels, vocalic r/l) decompose
-- into a token sequence; see 'unicodeDecompose'.
tokenizeUnicode :: Text -> [Token]
tokenizeUnicode input = go 0 input
  where
    go _ rest | T.null rest = []
    go offset rest =
        case T.uncons rest of
            Nothing -> []
            Just (c, next) ->
                let raw = T.singleton c
                    end = offset + 1
                    span = mkSpan (fromIntegral offset) (fromIntegral end)
                 in classifyChar span raw c <> go end next

classifyChar :: Span -> Text -> Char -> [Token]
classifyChar span raw ch =
    case canonicalSeq ch of
        Just canons -> mkSequenceTokens TsUnicode span raw canons
        Nothing -> [mkUnknown TsUnicode span raw]

-- | Canonical token sequence for a single character: either one canonical or
-- a decomposition into several (deprecated precomposed forms).
canonicalSeq :: Char -> Maybe [TokenCanonical]
canonicalSeq ch =
    case lookup ch unicodeDecompose of
        Just canons -> Just canons
        Nothing -> fmap pure (classifyCanonical ch)

-- | Deprecated precomposed Tibetan characters decomposed to canonical token
-- sequences on input: aspirated consonants (± subjoined), long vowels and
-- vocalic r/l. Matches the reference decompositions (ewts-converter / jsewts).
unicodeDecompose :: [(Char, [TokenCanonical])]
unicodeDecompose =
    [ ('\x0f43', [TcConsonant Cg, TcSubConsonant SCh])
    , ('\x0f4d', [TcConsonant CD, TcSubConsonant SCh])
    , ('\x0f52', [TcConsonant Cd, TcSubConsonant SCh])
    , ('\x0f57', [TcConsonant Cb, TcSubConsonant SCh])
    , ('\x0f5c', [TcConsonant Cdz, TcSubConsonant SCh])
    , ('\x0f69', [TcConsonant Ck, TcSubConsonant SCSh])
    , ('\x0f93', [TcSubConsonant SCg, TcSubConsonant SCh])
    , ('\x0f9d', [TcSubConsonant SCD, TcSubConsonant SCh])
    , ('\x0fa2', [TcSubConsonant SCd, TcSubConsonant SCh])
    , ('\x0fa7', [TcSubConsonant SCb, TcSubConsonant SCh])
    , ('\x0fac', [TcSubConsonant SCdz, TcSubConsonant SCh])
    , ('\x0fb9', [TcSubConsonant SCk, TcSubConsonant SCSh])
    , ('\x0f73', [TcVowel VA, TcVowel Vi])
    , ('\x0f75', [TcVowel VA, TcVowel Vu])
    , ('\x0f81', [TcVowel VA, TcVowel V_i])
    , ('\x0f76', [TcSubConsonant SCr, TcVowel V_i])
    , ('\x0f77', [TcSubConsonant SCr, TcVowel VA, TcVowel V_i])
    , ('\x0f78', [TcSubConsonant SCl, TcVowel V_i])
    , ('\x0f79', [TcSubConsonant SCl, TcVowel VA, TcVowel V_i])
    ]

-- | Map a single character to its shared canonical payload, via the same
-- inverse tables used by 'classifyChar'.
classifyCanonical :: Char -> Maybe TokenCanonical
classifyCanonical ch =
    asum
        [ TcConsonant <$> inverseUnicodeConsonant raw
        , TcSubConsonant <$> inverseUnicodeSubConsonant raw
        , TcVowel <$> inverseUnicodeVowel raw
        , TcFinal <$> inverseUnicodeFinal raw
        , TcNumber <$> inverseUnicodeNumber raw
        , TcHalfNumber <$> inverseUnicodeHalfNumber raw
        , TcPunctuation <$> inverseUnicodePunctuation raw
        , TcSign <$> inverseUnicodeSign raw
        , TcSanskritMark <$> inverseUnicodeSanskritMark raw
        , TcOrnament <$> inverseUnicodeOrnament raw
        , TcSymbol <$> inverseUnicodeSymbol raw
        , TcSpace <$> inverseUnicodeSpace raw
        ]
  where
    raw = T.singleton ch

-- | Render a canonical token to its Unicode spelling.
unicodeOf :: TokenCanonical -> Maybe Text
unicodeOf = \case
    TcConsonant c -> Just (unicodeConsonant c)
    TcSubConsonant s -> Just (unicodeSubConsonant s)
    TcVowel v -> Just (unicodeVowel v)
    TcFinal f -> Just (unicodeFinal f)
    TcNumber n -> Just (unicodeNumber n)
    TcHalfNumber h -> Just (unicodeHalfNumber h)
    TcPunctuation p -> Just (unicodePunctuation p)
    TcSign s -> Just (unicodeSign s)
    TcSanskritMark m -> Just (unicodeSanskritMark m)
    TcOrnament o -> Just (unicodeOrnament o)
    TcSpace m -> Just (unicodeSpace m)
    TcSymbol s -> Just (unicodeSymbol s)
    TcConSpec _ -> Nothing
    TcUnknown _ -> Nothing

unicodeConsonant :: Consonant -> Text
unicodeConsonant = \case
    Ck -> "\x0f40"
    Ckh -> "\x0f41"
    Cg -> "\x0f42"
    Cng -> "\x0f44"
    Cc -> "\x0f45"
    Cch -> "\x0f46"
    Cj -> "\x0f47"
    Cny -> "\x0f49"
    CT -> "\x0f4a"
    CTh -> "\x0f4b"
    CD -> "\x0f4c"
    CN -> "\x0f4e"
    Ct -> "\x0f4f"
    Cth -> "\x0f50"
    Cd -> "\x0f51"
    Cn -> "\x0f53"
    Cp -> "\x0f54"
    Cph -> "\x0f55"
    Cb -> "\x0f56"
    Cm -> "\x0f58"
    Cts -> "\x0f59"
    Ctsh -> "\x0f5a"
    Cdz -> "\x0f5b"
    Cw -> "\x0f5d"
    Czh -> "\x0f5e"
    Cz -> "\x0f5f"
    C' -> "\x0f60"
    Cy -> "\x0f61"
    Cr -> "\x0f62"
    Cl -> "\x0f63"
    Csh -> "\x0f64"
    CSh -> "\x0f65"
    Cs -> "\x0f66"
    Ch -> "\x0f67"
    Ca -> "\x0f68"
    CR -> "\x0f6a"
    Ckka -> "\x0f6b"
    CRra -> "\x0f6c"

unicodeSubConsonant :: SubConsonant -> Text
unicodeSubConsonant = \case
    SCk -> "\x0f90"
    SCkh -> "\x0f91"
    SCg -> "\x0f92"
    SCng -> "\x0f94"
    SCc -> "\x0f95"
    SCch -> "\x0f96"
    SCj -> "\x0f97"
    SCny -> "\x0f99"
    SCT -> "\x0f9a"
    SCTh -> "\x0f9b"
    SCD -> "\x0f9c"
    SCN -> "\x0f9e"
    SCt -> "\x0f9f"
    SCth -> "\x0fa0"
    SCd -> "\x0fa1"
    SCn -> "\x0fa3"
    SCp -> "\x0fa4"
    SCph -> "\x0fa5"
    SCb -> "\x0fa6"
    SCm -> "\x0fa8"
    SCts -> "\x0fa9"
    SCtsh -> "\x0faa"
    SCdz -> "\x0fab"
    SCw -> "\x0fad"
    SCzh -> "\x0fae"
    SCz -> "\x0faf"
    SC' -> "\x0fb0"
    SCy -> "\x0fb1"
    SCr -> "\x0fb2"
    SCl -> "\x0fb3"
    SCsh -> "\x0fb4"
    SCSh -> "\x0fb5"
    SCs -> "\x0fb6"
    SCh -> "\x0fb7"
    SCa -> "\x0fb8"
    SCW -> "\x0fba"
    SCY -> "\x0fbb"
    SCR -> "\x0fbc"

unicodeVowel :: Vowel -> Text
unicodeVowel = \case
    VA -> "\x0f71"
    Vi -> "\x0f72"
    Vu -> "\x0f74"
    Ve -> "\x0f7a"
    Vai -> "\x0f7b"
    Vo -> "\x0f7c"
    Vau -> "\x0f7d"
    V_i -> "\x0f80"

unicodeFinal :: FinalMark -> Text
unicodeFinal = \case
    FMAnusvara -> "\x0f7e"
    FMBinduNada -> "\x0f82"
    FMCandrabindu -> "\x0f83"
    FMSrogMed -> "\x0f37"
    FMCandrabinduHalanta -> "\x0f35"
    FMVisarga -> "\x0f7f"
    FMHalanta -> "\x0f84"
    FMCaret -> "\x0f39"
    FMYigMgo -> "\x0f85"

unicodeNumber :: Number -> Text
unicodeNumber = \case
    N0 -> "\x0f20"
    N1 -> "\x0f21"
    N2 -> "\x0f22"
    N3 -> "\x0f23"
    N4 -> "\x0f24"
    N5 -> "\x0f25"
    N6 -> "\x0f26"
    N7 -> "\x0f27"
    N8 -> "\x0f28"
    N9 -> "\x0f29"

unicodeHalfNumber :: HalfNumber -> Text
unicodeHalfNumber = \case
    H_0 -> "\x0f33"
    H_1 -> "\x0f2a"
    H_2 -> "\x0f2b"
    H_3 -> "\x0f2c"
    H_4 -> "\x0f2d"
    H_5 -> "\x0f2e"
    H_6 -> "\x0f2f"
    H_7 -> "\x0f30"
    H_8 -> "\x0f31"
    H_9 -> "\x0f32"

unicodePunctuation :: PunctuationMark -> Text
unicodePunctuation = \case
    PMTsheg -> "\x0f0b"
    PMNonBreakingTsheg -> "\x0f0c"
    PMShad -> "\x0f0d"
    PMNyisShad -> "\x0f0e"
    PMTshegShad -> "\x0f0f"
    PMNyisTshegShad -> "\x0f10"
    PMRinChenSpungsShad -> "\x0f11"
    PMRgyaGramShad -> "\x0f12"
    PMCaretDzudRtagsMeLong -> "\x0f13"
    PMGterTshigMgo -> "\x0f14"

unicodeSign :: SignMark -> Text
unicodeSign = \case
    SGYigMgoAt -> "\x0f00"
    SGKaKhaGaGsum -> "\x0f01"
    SGNyiZlaNaaDa -> "\x0f02"
    SGSbrulShad -> "\x0f03"

unicodeSanskritMark :: SanskritMark -> Text
unicodeSanskritMark = \case
    SMiLciRtags -> "\x0f86"
    SMiYangRtags -> "\x0f87"
    SMiLceTsaCanSubjoined -> "\x0f8d"
    SMiMchuCanSubjoined -> "\x0f8e"
    SMiInvertedMchuCanSubjoined -> "\x0f8f"

unicodeOrnament :: OrnamentMark -> Text
unicodeOrnament = \case
    OMRdelDkarGcig -> "\x0fd0"
    OMRdelDkarGnyis -> "\x0fd1"
    OMRdelDkarGsum -> "\x0fd2"
    OMRdelNagGcig -> "\x0fd3"
    OMRdelNagGnyis -> "\x0fd4"
    OMLeadingMchanRtags -> "\x0fd9"
    OMTrailingMchanRtags -> "\x0fda"

unicodeSpace :: SpaceMark -> Text
unicodeSpace = \case
    SMSpace -> " "

unicodeSymbol :: SymbolMark -> Text
unicodeSymbol = \case
    SMExclamation -> "\x0f08"
    SMAt -> "\x0f04"
    SMHash -> "\x0f05"
    SMDollar -> "\x0f06"
    SMPercent -> "\x0f07"
    SMEqual -> "\x0f34"
    SMLt -> "\x0f3a"
    SMGt -> "\x0f3b"
    SMLParen -> "\x0f3c"
    SMRParen -> "\x0f3d"

inverseUnicodeConsonant :: Text -> Maybe Consonant
inverseUnicodeConsonant = inverseMap unicodeConsonant

inverseUnicodeSubConsonant :: Text -> Maybe SubConsonant
inverseUnicodeSubConsonant = inverseMap unicodeSubConsonant

inverseUnicodeVowel :: Text -> Maybe Vowel
inverseUnicodeVowel = inverseMap unicodeVowel

inverseUnicodeFinal :: Text -> Maybe FinalMark
inverseUnicodeFinal = inverseMap unicodeFinal

inverseUnicodeNumber :: Text -> Maybe Number
inverseUnicodeNumber = inverseMap unicodeNumber

inverseUnicodeHalfNumber :: Text -> Maybe HalfNumber
inverseUnicodeHalfNumber = inverseMap unicodeHalfNumber

inverseUnicodePunctuation :: Text -> Maybe PunctuationMark
inverseUnicodePunctuation = inverseMap unicodePunctuation

inverseUnicodeSign :: Text -> Maybe SignMark
inverseUnicodeSign = inverseMap unicodeSign

inverseUnicodeSanskritMark :: Text -> Maybe SanskritMark
inverseUnicodeSanskritMark = inverseMap unicodeSanskritMark

inverseUnicodeOrnament :: Text -> Maybe OrnamentMark
inverseUnicodeOrnament = inverseMap unicodeOrnament

inverseUnicodeSpace :: Text -> Maybe SpaceMark
inverseUnicodeSpace = inverseMap unicodeSpace

inverseUnicodeSymbol :: Text -> Maybe SymbolMark
inverseUnicodeSymbol = inverseMap unicodeSymbol