module Convert.Token where

import Data.Char (chr, isHexDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural (Natural)
import Numeric (readHex)
import Text.Printf (printf)

-- | Превращает строку в формат "\\x0f40\\x0fad..."
toUnicodeEscape :: String -> String
toUnicodeEscape = concatMap charToHex
    where
        charToHex c = printf "\\x%04x" (ord c)

-- >>> toUnicodeEscape "ཨོུ"
-- "\\x0f68\\x0f74\\x0f7c"

fromUnicodeEscape :: String -> String
fromUnicodeEscape [] = []
fromUnicodeEscape ('\\' : 'x' : a : b : c : d : rest)
    | all isHexDigit [a, b, c, d] =
        let [(val, "")] = readHex [a, b, c, d]
         in chr val : fromUnicodeEscape rest
fromUnicodeEscape (x : xs) = x : fromUnicodeEscape xs

-- IR input source.
data TokenSource
    = TsUnicode
    | TsWylie
    deriving (Show, Eq)

-- High-level token classes shared by both tokenizers.
data TokenKind
    = TkConsonant
    | TkSubConsonant
    | TkVowel
    | TkFinal
    | TkNumber
    | TkHalfNumber
    | TkPunctuation
    | TkSign
    | TkSanskritMark
    | TkOrnament
    | TkSpace
    | TkSymbol
    | TkUnknown
    deriving (Show, Eq)

-- Whether raw spelling should be preserved when alias forms normalize.
data AliasPolicy
    = PreserveRaw
    | Canonicalized
    deriving (Show, Eq)

data TokenIssueCode
    = UnknownChar
    | InvalidSequence
    | AmbiguousAlias
    | AutoNormalized
    deriving (Show, Eq)

data TokenIssueSeverity
    = TisWarning
    | TisError
    deriving (Show, Eq)

data TokenIssue = TokenIssue
    { issueCode :: TokenIssueCode
    , issueSeverity :: TokenIssueSeverity
    , issueMessage :: Text
    }
    deriving (Show, Eq)

-- Character offsets in the original input, half-open interval [start, end).
data Span = Span
    { offsetStart :: Natural
    , offsetEnd :: Natural
    }
    deriving (Show, Eq)

-- Keep spans half-open and monotonic: [start, end), end >= start.
mkSpan :: Natural -> Natural -> Span
mkSpan start end =
    let end' = max start end
    in Span start end'

-- Canonical typed payload of an IR token.
data TokenCanonical
    = TcConsonant Consonant
    | TcSubConsonant SubConsonant
    | TcVowel Vowel
    | TcFinal FinalMark
    | TcNumber Number
    | TcHalfNumber HalfNumber
    | TcPunctuation PunctuationMark
    | TcSign SignMark
    | TcSanskritMark SanskritMark
    | TcOrnament OrnamentMark
    | TcSpace SpaceMark
    | TcSymbol SymbolMark
    | TcUnknown UnknownMark
    deriving (Show, Eq)

-- Canonical IR token used as contract between tokenizer and grammar layers.
-- Invariants:
-- 1) tokenRaw always stores the exact source slice from input.
-- 2) tokenCanonical is always normalized to shared canonical domain values.
-- 3) tokenSpan is a half-open interval [start, end) over source offsets.
-- 4) tokenIssues only describe lexical/tokenization-level diagnostics.
data Token = Token
    { tokenRaw :: Text
    , tokenCanonical :: TokenCanonical
    , tokenKind :: TokenKind
    , tokenSource :: TokenSource
    , tokenSpan :: Span
    , tokenAliasPolicy :: AliasPolicy
    , tokenIssues :: [TokenIssue]
    }
    deriving (Show, Eq)

-- Smart constructors centralize Token invariants for both tokenizers.
mkTokenWith :: TokenSource -> TokenKind -> Text -> TokenCanonical -> Span -> AliasPolicy -> [TokenIssue] -> Token
mkTokenWith source kind raw canonical span aliasPolicy issues =
    Token
        { tokenRaw = raw
        , tokenCanonical = canonical
        , tokenKind = kind
        , tokenSource = source
        , tokenSpan = mkSpan (offsetStart span) (offsetEnd span)
        , tokenAliasPolicy = aliasPolicy
        , tokenIssues = issues
        }

mkToken :: TokenSource -> TokenKind -> Text -> TokenCanonical -> Span -> Token
mkToken source kind raw canonical span =
    mkTokenWith source kind raw canonical span PreserveRaw []

mkConsonant :: TokenSource -> Span -> Text -> Consonant -> Token
mkConsonant source span raw consonant =
    mkToken source TkConsonant raw (TcConsonant consonant) span

mkSubConsonant :: TokenSource -> Span -> Text -> SubConsonant -> Token
mkSubConsonant source span raw subConsonant =
    mkToken source TkSubConsonant raw (TcSubConsonant subConsonant) span

mkVowel :: TokenSource -> Span -> Text -> Vowel -> Token
mkVowel source span raw vowel =
    mkToken source TkVowel raw (TcVowel vowel) span

mkFinal :: TokenSource -> Span -> Text -> FinalMark -> Token
mkFinal source span raw finalMark =
    mkToken source TkFinal raw (TcFinal finalMark) span

mkNumber :: TokenSource -> Span -> Text -> Number -> Token
mkNumber source span raw number =
    mkToken source TkNumber raw (TcNumber number) span

mkHalfNumber :: TokenSource -> Span -> Text -> HalfNumber -> Token
mkHalfNumber source span raw halfNumber =
    mkToken source TkHalfNumber raw (TcHalfNumber halfNumber) span

mkPunctuation :: TokenSource -> Span -> Text -> PunctuationMark -> Token
mkPunctuation source span raw punctuationMark =
    mkToken source TkPunctuation raw (TcPunctuation punctuationMark) span

mkSign :: TokenSource -> Span -> Text -> SignMark -> Token
mkSign source span raw signMark =
    mkToken source TkSign raw (TcSign signMark) span

mkSanskritMark :: TokenSource -> Span -> Text -> SanskritMark -> Token
mkSanskritMark source span raw sanskritMark =
    mkToken source TkSanskritMark raw (TcSanskritMark sanskritMark) span

mkOrnament :: TokenSource -> Span -> Text -> OrnamentMark -> Token
mkOrnament source span raw ornamentMark =
    mkToken source TkOrnament raw (TcOrnament ornamentMark) span

mkSpace :: TokenSource -> Span -> Text -> SpaceMark -> Token
mkSpace source span raw spaceMark =
    mkToken source TkSpace raw (TcSpace spaceMark) span

mkSymbol :: TokenSource -> Span -> Text -> SymbolMark -> Token
mkSymbol source span raw symbolMark =
    mkToken source TkSymbol raw (TcSymbol symbolMark) span

mkUnknownWith :: TokenSource -> Span -> Text -> [TokenIssue] -> Token
mkUnknownWith source span raw issues =
    mkTokenWith source TkUnknown raw (TcUnknown (UnknownMark raw)) span PreserveRaw issues

mkUnknown :: TokenSource -> Span -> Text -> Token
mkUnknown source span raw =
    mkUnknownWith source span raw [TokenIssue UnknownChar TisWarning (T.pack "Unknown token")]

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ fromUnicodeEscape "\\x0f74\\x0f7c"
-- "ོུ"

data Consonant
    = Ck -- ཀ \u0f40
    | Ckh -- ཁ \u0f41
    | Cg -- ག \u0f42
    | CgPLUSh -- གྷ \u0f43
    | Cng -- ང \u0f44
    | Cc -- ཅ \u0f45
    | Cch -- ཆ \u0f46
    | Cj -- ཇ \u0f47
    | Cny -- ཉ \u0f49
    | CT -- ཊ \u0f4a
    | CTh -- ཋ \u0f4b
    | CD -- ཌ \u0f4c
    | CDPLUSh -- ཌྷ \u0f4d
    | CN -- ཎ \u0f4e
    | Ct -- ཏ \u0f4f
    | Cth -- ཐ \u0f50
    | Cd -- ད \u0f51
    | CdPLUSh -- དྷ \u0f52
    | Cn -- ན \u0f53
    | Cp -- པ \u0f54
    | Cph -- ཕ \u0f55
    | Cf -- f (EWTS-specific) ཕ༹ \u0f55\u0f39
    | Cb -- བ \u0f56
    | Cv -- v (EWTS-specific) བ༹ \u0f56\u0f39
    | CbPLUSh -- བྷ \u0f57
    | Cm -- མ \u0f58
    | Cts -- ཙ \u0f59
    | Ctsh -- ཚ \u0f5a
    | Cdz -- ཛ \u0f5b
    | CdzPLUSh -- ཛྷ \u0f5c
    | Cw -- ཝ \u0f5d
    | Czh -- ཞ \u0f5e
    | Cz -- ཟ \u0f5f
    | C' -- འ \u0f60
    | Cy -- ཡ \u0f61
    | Cr -- ར \u0f62
    | Cl -- ལ \u0f63
    | Csh -- ཤ \u0f64
    | CSh -- ཥ \u0f65
    | Cs -- ས \u0f66
    | Ch -- ཧ \u0f67
    | Ca -- ཨ \u0f68
    | CkPLUSSh -- ཀྵ \u0f69
    | CR -- ཪ \u0f6a
    | Ckka -- ཫ \u0f6b
    | CRra -- ཬ \u0f6c
    deriving (Show, Eq)

data Vowel
    = VA -- ཱ \u0f71
    | Vi -- ི \u0f72
    | VI -- ཱི \u0f73
    | Vu -- ུ \u0f74
    | VU -- ཱུ \u0f75
    | Vr_i -- ྲྀ \u0f76
    | Vr_I -- ཷ \u0f77
    | Vl_i -- ླྀ \u0f78
    | Vl_I -- ཹ \u0f79
    | Ve -- ེ \u0f7a
    | Vai -- ཻ \u0f7b
    | Vo -- ོ \u0f7c
    | Vau -- ཽ \u0f7d
    | V_i -- ྀ \u0f80
    | V_I -- ཱྀ \u0f81

    -- | Vuo -- ོུ \u0f74\u0f7c
    -- | Vui -- ིུ \u0f74\u0f72
    -- | Vue -- ེུ \u0f74\u0f7a
    deriving (Show, Eq)

data Number
    = N0 -- ༠ \u0f20
    | N1 -- ༡ \u0f21
    | N2 -- ༢ \u0f22
    | N3 -- ༣ \u0f23
    | N4 -- ༤ \u0f24
    | N5 -- ༥ \u0f25
    | N6 -- ༦ \u0f26
    | N7 -- ༧ \u0f27
    | N8 -- ༨ \u0f28
    | N9 -- ༩ \u0f29
    deriving (Show, Eq)

data HalfNumber
    = H_0 -- ༳ \u0f33
    | H_1 -- ༪ \u0f2a
    | H_2 -- ༫ \u0f2b
    | H_3 -- ༬ \u0f2c
    | H_4 -- ༭ \u0f2d
    | H_5 -- ༮ \u0f2e
    | H_6 -- ༯ \u0f2f
    | H_7 -- ༰ \u0f30
    | H_8 -- ༱ \u0f31
    | H_9 -- ༲ \u0f32
    deriving (Show, Eq)

-- Subjoined Tibetan consonants used in stacks.
data SubConsonant
    = SCk -- ྐ \u0f90
    | SCkh -- ྑ \u0f91
    | SCg -- ྒ \u0f92
    | SCgPLUSh -- ྒྷ \u0f93
    | SCng -- ྔ \u0f94
    | SCc -- ྕ \u0f95
    | SCch -- ྖ \u0f96
    | SCj -- ྗ \u0f97
    | SCny -- ྙ \u0f99
    | SCT -- ྚ \u0f9a
    | SCTh -- ྛ \u0f9b
    | SCD -- ྜ \u0f9c
    | SCDPLUSh -- ྜྷ \u0f9d
    | SCN -- ྞ \u0f9e
    | SCt -- ྟ \u0f9f
    | SCth -- ྠ \u0fa0
    | SCd -- ྡ \u0fa1
    | SCdPLUSh -- ྡྷ \u0fa2
    | SCn -- ྣ \u0fa3
    | SCp -- ྤ \u0fa4
    | SCph -- ྥ \u0fa5
    | SCb -- ྦ \u0fa6
    | SCbPLUSh -- ྦྷ \u0fa7
    | SCm -- ྨ \u0fa8
    | SCts -- ྩ \u0fa9
    | SCtsh -- ྪ \u0faa
    | SCdz -- ྫ \u0fab
    | SCdzPLUSh -- ྫྷ \u0fac
    | SCw -- ྭ \u0fad
    | SCzh -- ྮ \u0fae
    | SCz -- ྯ \u0faf
    | SC' -- ྰ \u0fb0
    | SCy -- ྱ \u0fb1
    | SCr -- ྲ \u0fb2
    | SCl -- ླ \u0fb3
    | SCsh -- ྴ \u0fb4
    | SCSh -- ྵ \u0fb5
    | SCs -- ྶ \u0fb6
    | SCh -- ྷ \u0fb7
    | SCa -- ྸ \u0fb8
    | SCkPLUSSh -- ྐྵ \u0fb9
    | SCW -- ྺ \u0fba
    | SCY -- ྻ \u0fbb
    | SCR -- ྼ \u0fbc
    deriving (Show, Eq)

data FinalMark
    = FMAnusvara -- ཾ \u0f7e, ྂ \u0f82, ྃ \u0f83
    | FMVisarga -- ཿ \u0f7f
    | FMCandrabinduOrNasal -- ༵ \u0f35, ༷ \u0f37
    | FMHalanta -- ྄ \u0f84
    | FMCaret -- ྐྵ \u0f39
    | FMYigMgo -- ྅ \u0f85
    deriving (Show, Eq)

data PunctuationMark
    = PMTsheg -- ་ \u0f0b
    | PMNonBreakingTsheg -- ༌ \u0f0c
    | PMShad -- ། \u0f0d
    | PMNyisShad -- ༎ \u0f0e
    | PMTshegShad -- ༏ \u0f0f
    | PMNyisTshegShad -- ༐ \u0f10
    | PMRinChenSpungsShad -- ༑ \u0f11
    | PMRgyaGramShad -- ༒ \u0f12
    | PMCaretDzudRtagsMeLong -- ༓ \u0f13
    | PMGterTshigMgo -- ༔ \u0f14
    deriving (Show, Eq)

data SignMark
    = SGYigMgoAt -- ༀ \u0f00
    | SGKaKhaGaGsum -- ༁ \u0f01
    | SGNyiZlaNaaDa -- ༂ \u0f02
    | SGSbrulShad -- ༃ \u0f03
    deriving (Show, Eq)

data SanskritMark
    = SMiLciRtags -- ྆ \u0f86
    | SMiYangRtags -- ྇ \u0f87
    | SMiLceTsaCanSubjoined -- ྍ \u0f8d
    | SMiMchuCanSubjoined -- ྎ \u0f8e
    | SMiInvertedMchuCanSubjoined -- ྏ \u0f8f
    deriving (Show, Eq)

data OrnamentMark
    = OMRdelDkarGcig -- ࿐ \u0fd0
    | OMRdelDkarGnyis -- ࿑ \u0fd1
    | OMRdelDkarGsum -- ࿒ \u0fd2
    | OMRdelNagGcig -- ࿓ \u0fd3
    | OMRdelNagGnyis -- ࿔ \u0fd4
    | OMLeadingMchanRtags -- ࿙ \u0fd9
    | OMTrailingMchanRtags -- ࿚ \u0fda
    deriving (Show, Eq)

data SpaceMark
    = SMSpace --   \u0020
    deriving (Show, Eq)

data SymbolMark
    = SMExclamation -- ! \u0021
    | SMAt -- @ \u0040
    | SMHash -- # \u0023
    | SMDollar -- $ \u0024
    | SMPercent -- % \u0025
    | SMEqual -- = \u003d
    | SMLt -- < \u003c
    | SMGt -- > \u003e
    | SMLParen -- ( \u0028
    | SMRParen -- ) \u0029
    | SMAsterisk -- * \u002a
    | SMSlash -- / \u002f
    | SMDoubleSlash -- // \u002f\u002f
    | SMSemicolon -- ; \u003b
    | SMBar -- | \u007c
    | SMColon -- : \u003a
    deriving (Show, Eq)

newtype UnknownMark = UnknownMark Text
    deriving (Show, Eq)

-- >>> import Data.Char
-- >>> import qualified Data.Text as T
-- >>> T.map ((\c -> (c, ord c))) "ཀཁགངཅཆཇཉཏཐདནཔཕབམཙཚཛཝཞཟའཡརལཤསཧཨ"
-- Couldn't match expected type `Char' with actual type `(Char, Int)'
-- In the expression: (c_an4V, ord c_an4V)
-- In the first argument of `map', namely
--   `((\ c_an4V -> (c_an4V, ord c_an4V)))'
-- In the expression:
--   map
--     ((\ c_an4V -> (c_an4V, ord c_an4V)))
--     "ཀཁགངཅཆཇཉཏཐདནཔཕབམཙཚཛཝཞཟའཡརལཤསཧཨ"