module Convert.Token where

import Data.Char (chr, isHexDigit, ord)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Numeric (readHex)
import Numeric.Natural (Natural)
import Text.Printf (printf)

-- | Partial inverse of @f@, mirroring Relude 'Relude.Enum.inverseMap'.
-- The resulting @Map k a@ is built once and shared for every call.
inverseMap :: (Bounded a, Enum a, Ord k) => (a -> k) -> (k -> Maybe a)
inverseMap f = \k -> M.lookup k dict
    where
        dict = M.fromList [(f a, a) | a <- [minBound .. maxBound]]

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
        case readHex [a, b, c, d] of
            [(val, "")] -> chr val : fromUnicodeEscape rest
            _ -> '\\' : fromUnicodeEscape ('x' : a : b : c : d : rest)
fromUnicodeEscape (x : xs) = x : fromUnicodeEscape xs

-- IR input source.
data TokenSource
    = TsUnicode
    | TsWylie
    deriving (Show, Eq, Ord)

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
    | TkConSpec
    | TkNonTibetan
    | TkUnknown
    deriving (Show, Eq, Ord)

-- Whether raw spelling should be preserved when alias forms normalize.
data AliasPolicy
    = PreserveRaw
    | Canonicalized
    deriving (Show, Eq, Ord)

-- Character offsets in the original input, half-open interval [start, end).
data Span = Span
    { offsetStart :: Natural
    , offsetEnd :: Natural
    }
    deriving (Show, Eq, Ord)

-- Keep sps half-open and monotonic: [start, end), end >= start.
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
    | TcConSpec ConSpec
    | TcUnknown UnknownMark
    deriving (Show, Eq, Ord)

-- Canonical IR token used as contract between tokenizer and grammar layers.
-- Invariants:
-- 1) tokenRaw stores the exact source slice from input; a token that is a
--    *continuation* of a decomposed/expanded source slice (one slice ->
--    several tokens) stores the empty slice at the slice end instead.
-- 2) tokenCanonical is always normalized to shared canonical domain values
--    (compound forms; deprecated precomposed spellings never occur here).
-- 3) tokenSpan is a half-open interval [start, end) over source offsets;
--    sps are monotonic and cover the whole input without gaps or overlap.
data Token = Token
    { tokenRaw :: Text
    , tokenCanonical :: TokenCanonical
    , tokenKind :: TokenKind
    , tokenSource :: TokenSource
    , tokenSpan :: Span
    , tokenAliasPolicy :: AliasPolicy
    }
    deriving (Show, Eq, Ord)

-- Smart constructors centralize Token invariants for both tokenizers.
mkTokenWith ::
    TokenSource
    -> TokenKind
    -> Text
    -> TokenCanonical
    -> Span
    -> AliasPolicy
    -> Token
mkTokenWith source kind raw canonical sp aliasPolicy =
    Token
        { tokenRaw = raw
        , tokenCanonical = canonical
        , tokenKind = kind
        , tokenSource = source
        , tokenSpan = mkSpan (offsetStart sp) (offsetEnd sp)
        , tokenAliasPolicy = aliasPolicy
        }

mkToken :: TokenSource -> TokenKind -> Text -> TokenCanonical -> Span -> Token
mkToken source kind raw canonical sp =
    mkTokenWith source kind raw canonical sp PreserveRaw

mkConsonant :: TokenSource -> Span -> Text -> Consonant -> Token
mkConsonant source sp raw consonant =
    mkToken source TkConsonant raw (TcConsonant consonant) sp

mkSubConsonant :: TokenSource -> Span -> Text -> SubConsonant -> Token
mkSubConsonant source sp raw subConsonant =
    mkToken source TkSubConsonant raw (TcSubConsonant subConsonant) sp

mkVowel :: TokenSource -> Span -> Text -> Vowel -> Token
mkVowel source sp raw vowel =
    mkToken source TkVowel raw (TcVowel vowel) sp

mkFinal :: TokenSource -> Span -> Text -> FinalMark -> Token
mkFinal source sp raw finalMark =
    mkToken source TkFinal raw (TcFinal finalMark) sp

mkNumber :: TokenSource -> Span -> Text -> Number -> Token
mkNumber source sp raw number =
    mkToken source TkNumber raw (TcNumber number) sp

mkHalfNumber :: TokenSource -> Span -> Text -> HalfNumber -> Token
mkHalfNumber source sp raw halfNumber =
    mkToken source TkHalfNumber raw (TcHalfNumber halfNumber) sp

mkPunctuation :: TokenSource -> Span -> Text -> PunctuationMark -> Token
mkPunctuation source sp raw punctuationMark =
    mkToken source TkPunctuation raw (TcPunctuation punctuationMark) sp

mkSign :: TokenSource -> Span -> Text -> SignMark -> Token
mkSign source sp raw signMark =
    mkToken source TkSign raw (TcSign signMark) sp

mkSanskritMark :: TokenSource -> Span -> Text -> SanskritMark -> Token
mkSanskritMark source sp raw sanskritMark =
    mkToken source TkSanskritMark raw (TcSanskritMark sanskritMark) sp

mkOrnament :: TokenSource -> Span -> Text -> OrnamentMark -> Token
mkOrnament source sp raw ornamentMark =
    mkToken source TkOrnament raw (TcOrnament ornamentMark) sp

mkSpace :: TokenSource -> Span -> Text -> SpaceMark -> Token
mkSpace source sp raw spaceMark =
    mkToken source TkSpace raw (TcSpace spaceMark) sp

mkSymbol :: TokenSource -> Span -> Text -> SymbolMark -> Token
mkSymbol source sp raw symbolMark =
    mkToken source TkSymbol raw (TcSymbol symbolMark) sp

mkUnknown :: TokenSource -> Span -> Text -> Token
mkUnknown source sp raw =
    mkToken source TkUnknown raw (TcUnknown (UnknownMark raw)) sp

-- | A bracketed block of non-Wylie text. The brackets are Wylie-only syntax:
-- they stay in the raw slice (so the input remains fully covered), while the
-- content - the text the token stands for - is what reaches the other script.
mkNonTibetan :: TokenSource -> Span -> Text -> Text -> Token
mkNonTibetan source sp raw content =
    mkToken source TkNonTibetan raw (TcUnknown (UnknownMark content)) sp

-- | Build a 'Token' from a decoded canonical payload, dispatching on its
-- constructor. Used by both tokenizers when a raw chunk (e.g. a \\uXXXX
-- escape) resolves to a known canonical.
mkTokenFromCanonical :: TokenSource -> Span -> Text -> TokenCanonical -> Token
mkTokenFromCanonical source sp raw = \case
    TcConsonant c -> mkConsonant source sp raw c
    TcSubConsonant s -> mkSubConsonant source sp raw s
    TcVowel v -> mkVowel source sp raw v
    TcFinal f -> mkFinal source sp raw f
    TcNumber n -> mkNumber source sp raw n
    TcHalfNumber h -> mkHalfNumber source sp raw h
    TcPunctuation p -> mkPunctuation source sp raw p
    TcSign s -> mkSign source sp raw s
    TcSanskritMark m -> mkSanskritMark source sp raw m
    TcOrnament o -> mkOrnament source sp raw o
    TcSpace m -> mkSpace source sp raw m
    TcSymbol s -> mkSymbol source sp raw s
    TcConSpec cs -> mkToken source TkConSpec raw (TcConSpec cs) sp
    TcUnknown _ -> mkUnknown source sp raw

-- | Build the token stream for one source slice that decomposes or expands
-- into several canonical tokens (e.g. "gh" -> [Cg, SCh], or a deprecated
-- precomposed Unicode char). The first token carries the slice and its span;
-- each continuation token carries an empty 'tokenRaw' and an empty span
-- sitting at the end of the slice, so that 'T.concat' over 'tokenRaw' always
-- reproduces the input exactly.
mkSequenceTokens :: TokenSource -> Span -> Text -> [TokenCanonical] -> [Token]
mkSequenceTokens source sp raw = \case
    [] -> []
    c : cs ->
        mkTokenFromCanonical source sp raw c
            : [mkTokenFromCanonical source (mkSpan end end) mempty k | k <- cs]
    where
        end = offsetEnd sp

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ fromUnicodeEscape "\\x0f74\\x0f7c"
-- "ོུ"

data Consonant
    = Ck -- ཀ \u0f40
    | Ckh -- ཁ \u0f41
    | Cg -- ག \u0f42
    | Cng -- ང \u0f44
    | Cc -- ཅ \u0f45
    | Cch -- ཆ \u0f46
    | Cj -- ཇ \u0f47
    | Cny -- ཉ \u0f49
    | CT -- ཊ \u0f4a
    | CTh -- ཋ \u0f4b
    | CD -- ཌ \u0f4c
    | CN -- ཎ \u0f4e
    | Ct -- ཏ \u0f4f
    | Cth -- ཐ \u0f50
    | Cd -- ད \u0f51
    | Cn -- ན \u0f53
    | Cp -- པ \u0f54
    | Cph -- ཕ \u0f55
    | Cb -- བ \u0f56
    | Cm -- མ \u0f58
    | Cts -- ཙ \u0f59
    | Ctsh -- ཚ \u0f5a
    | Cdz -- ཛ \u0f5b
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
    | CR -- ཪ \u0f6a
    | Ckka -- ཫ \u0f6b
    | CRra -- ཬ \u0f6c
    deriving (Show, Eq, Ord, Enum, Bounded)

-- The aspirated letters (gh, Dh, dh, bh, dzh) have no canonical of their own:
-- they are always the compound  C + subjoined-h  (e.g. "gh" = [Cg, SCh],
-- 0x0f42 0x0fb7). Deprecated precomposed codepoints (0x0f43, 0x0f4d, 0x0f52,
-- 0x0f57, 0x0f5c and their subjoined counterparts) are decomposed on input.
-- The same holds for the EWTS letters f/v: they are  C + caret  ([Cph, FMCaret]
-- / [Cb, FMCaret], 0x0f55 0x0f39 / 0x0f56 0x0f39), never canonicals of their
-- own.

data Vowel
    = VA -- ཱ \u0f71
    | Vi -- ི \u0f72
    | Vu -- ུ \u0f74
    | Ve -- ེ \u0f7a
    | Vai -- ཻ \u0f7b
    | Vo -- ོ \u0f7c
    | Vau -- ཽ \u0f7d
    | V_i -- ྀ \u0f80

    -- Long vowels and vocalic r/l have no canonicals of their own: they are
    -- token sequences over the atomic vowels above ("I" = [VA, Vi],
    -- "r-i" = [SCr, V_i], ...). Deprecated precomposed codepoints (0x0f73,
    -- 0x0f75, 0x0f76-0x0f79, 0x0f81) are decomposed on input.
    deriving (Show, Eq, Ord, Enum, Bounded)

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
    deriving (Show, Eq, Ord, Enum, Bounded)

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
    deriving (Show, Eq, Ord, Enum, Bounded)

-- Subjoined Tibetan consonants used in stacks.
data SubConsonant
    = SCk -- ྐ \u0f90
    | SCkh -- ྑ \u0f91
    | SCg -- ྒ \u0f92
    | SCng -- ྔ \u0f94
    | SCc -- ྕ \u0f95
    | SCch -- ྖ \u0f96
    | SCj -- ྗ \u0f97
    | SCny -- ྙ \u0f99
    | SCT -- ྚ \u0f9a
    | SCTh -- ྛ \u0f9b
    | SCD -- ྜ \u0f9c
    | SCN -- ྞ \u0f9e
    | SCt -- ྟ \u0f9f
    | SCth -- ྠ \u0fa0
    | SCd -- ྡ \u0fa1
    | SCn -- ྣ \u0fa3
    | SCp -- ྤ \u0fa4
    | SCph -- ྥ \u0fa5
    | SCb -- ྦ \u0fa6
    | SCm -- ྨ \u0fa8
    | SCts -- ྩ \u0fa9
    | SCtsh -- ྪ \u0faa
    | SCdz -- ྫ \u0fab
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
    | SCW -- ྺ \u0fba
    | SCY -- ྻ \u0fbb
    | SCR -- ྼ \u0fbc
    deriving (Show, Eq, Ord, Enum, Bounded)

-- The subjoined aspirated letters (SCgPLUSh 0x0f93, SCDPLUSh 0x0f9d,
-- SCdPLUSh 0x0fa2, SCbPLUSh 0x0fa7, SCdzPLUSh 0x0fac) are decomposed on input
-- into subjoined base + subjoined-h, mirroring the Consonant rule above.

-- | The nine EWTS final marks. 'finalSlot' files the eight sign marks in the
-- order a syllable's finals fill them; the caret takes no slot and keeps a
-- rule of its own (the repeated caret of Constraint21).
data FinalMark
    = FMAnusvara -- M ཾ \u0f7e
    | FMBinduNada -- ~M` ྂ \u0f82
    | FMCandrabindu -- ~M ྃ \u0f83
    | FMSrogMed -- X ༷ \u0f37 (sign ngas bzung nyi zla / srog med)
    | FMCandrabinduHalanta -- ~X ༵ \u0f35 (mark ngas bzung nyi zla)
    | FMVisarga -- H ཿ \u0f7f
    | FMHalanta -- ? ྄ \u0f84
    | -- | ༹ \u0f39
      FMCaret
    | FMYigMgo -- & ྅ \u0f85
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The slot a final sign fills in its syllable's closing chain, and Nothing
-- for the caret, which fills no slot.
--
-- A mark fills only its own slot, only once, and only while the chain has not
-- passed it. So @M~M`@ fills two slots in order and stands (baM~M`@ ->
-- བཾྂ), while @~M`M@ asks for a slot the chain has left behind and the mark
-- is lost (o~M`M@ -> ཨོྂ), and a mark that comes twice is lost the same way
-- (oMM@ -> ཨོཾ). The reference drops the mark and warns; we drop it from the
-- rendering and warn, which is what its own second caret already does here.
--
-- The order is ours, not the book's. The book has no word on the final marks
-- at all: its grammar 4.21 spells a word as prefix, superfix, root, subfix,
-- vowel sign, suffix and postfix, and Def 4.9 files the final marks with the
-- other signs rather than among the letters. What the reference corpora do fix
-- is @M@ before @~M`@, and @H@ before @X@ and before @~X@; the two orders the
-- other way and both repeats draw the warning. No corpus covers a third mark
-- in one chain, so the rest of the order stands on that reasoning, not on
-- evidence.
finalSlot :: FinalMark -> Maybe Int
finalSlot = \case
    FMAnusvara -> Just 1
    FMBinduNada -> Just 2
    FMCandrabindu -> Just 3
    FMVisarga -> Just 4
    FMSrogMed -> Just 5
    FMCandrabinduHalanta -> Just 6
    FMHalanta -> Just 7
    FMCaret -> Nothing
    FMYigMgo -> Just 8

-- | Wylie-only consonant-stack operators: '+' (explicit subjoin) and '.'
-- (explicit stack). They exist only in the Wylie input alphabet and have no
-- Unicode spelling.
data ConSpec
    = CSPlus
    | CSDot
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A consonant's subjoined form, when the register has a regular subjoined
-- letter for it. The bundled EWTS letters (kka ཫ, rra ཬ) have none: they are
-- stacks of their parts, never a subjoined letter of their own.
toSubjoined :: Consonant -> Maybe SubConsonant
toSubjoined = \case
    Ck -> Just SCk
    Ckh -> Just SCkh
    Cg -> Just SCg
    Cng -> Just SCng
    Cc -> Just SCc
    Cch -> Just SCch
    Cj -> Just SCj
    Cny -> Just SCny
    CT -> Just SCT
    CTh -> Just SCTh
    CD -> Just SCD
    CN -> Just SCN
    Ct -> Just SCt
    Cth -> Just SCth
    Cd -> Just SCd
    Cn -> Just SCn
    Cp -> Just SCp
    Cph -> Just SCph
    Cb -> Just SCb
    Cm -> Just SCm
    Cts -> Just SCts
    Ctsh -> Just SCtsh
    Cdz -> Just SCdz
    Cw -> Just SCw
    Czh -> Just SCzh
    Cz -> Just SCz
    C' -> Just SC'
    Cy -> Just SCy
    Cr -> Just SCr
    Cl -> Just SCl
    Csh -> Just SCsh
    CSh -> Just SCSh
    Cs -> Just SCs
    Ch -> Just SCh
    Ca -> Just SCa
    CR -> Just SCR
    Ckka -> Nothing
    CRra -> Nothing

-- | The subjoined form a token prints as. Source-aware: the EWTS raised
-- letters @W@ and @Y@ stay raised (their Wylie slices are @\"W\"@ and
-- @\"Y\"@), and a token that is already a subconsonant carries its own
-- subjoined form.
subjoinOf :: Token -> Maybe SubConsonant
subjoinOf Token{tokenRaw = raw, tokenCanonical = TcConsonant c}
    | raw == "W" = Just SCW
    | raw == "Y" = Just SCY
    | otherwise = toSubjoined c
subjoinOf Token{tokenCanonical = TcSubConsonant sc} = Just sc
subjoinOf _ = Nothing

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
    deriving (Show, Eq, Ord, Enum, Bounded)

data SignMark
    = SGYigMgoAt -- ༀ \u0f00
    | SGKaKhaGaGsum -- ༁ \u0f01
    | SGNyiZlaNaaDa -- ༂ \u0f02
    | SGSbrulShad -- ༃ \u0f03
    deriving (Show, Eq, Ord, Enum, Bounded)

data SanskritMark
    = SMiLciRtags -- ྆ \u0f86
    | SMiYangRtags -- ྇ \u0f87
    | SMiLceTsaCanSubjoined -- ྍ \u0f8d
    | SMiMchuCanSubjoined -- ྎ \u0f8e
    | SMiInvertedMchuCanSubjoined -- ྏ \u0f8f
    deriving (Show, Eq, Ord, Enum, Bounded)

data OrnamentMark
    = OMRdelDkarGcig -- ࿐ \u0fd0
    | OMRdelDkarGnyis -- ࿑ \u0fd1
    | OMRdelDkarGsum -- ࿒ \u0fd2
    | OMRdelNagGcig -- ࿓ \u0fd3
    | OMRdelNagGnyis -- ࿔ \u0fd4
    | OMLeadingMchanRtags -- ࿙ \u0fd9
    | OMTrailingMchanRtags -- ࿚ \u0fda
    deriving (Show, Eq, Ord, Enum, Bounded)

data SpaceMark
    = SMSpace --   \u0020
    deriving (Show, Eq, Ord, Enum, Bounded)

data SymbolMark
    = SMExclamation -- ! \u0021
    | SMAt -- @ \u0040
    | SMHash -- # \u0023
    | SMDollar
    | -- \$ \u0024
      SMPercent -- % \u0025
    | SMEqual -- = \u003d
    | SMLt -- < \u003c
    | SMGt -- > \u003e
    | SMLParen -- ( \u0028
    | SMRParen -- ) \u0029
    deriving (Show, Eq, Ord, Enum, Bounded)

newtype UnknownMark = UnknownMark Text
    deriving (Show, Eq, Ord)
