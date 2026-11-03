{- HLINT ignore "Use camelCase" -}

-- | The Wylie alphabet: how a canonical sign is written, plus the aliases
-- and compound spellings that read back as canonical tokens. Reading builds
-- its inverse maps from these tables and printing reads 'wylieOf', so the
-- two sides cannot drift apart.
module Convert.Token.Wylie where

import Convert.Token
import Data.Text (Text)

-- | Render a canonical token to its Wylie spelling.
wylieOf :: TokenCanonical -> Maybe Text
wylieOf = \case
    TcConsonant c -> Just (wylieConsonant c)
    -- A subjoined glyph has no spelling of its own in the Wylie alphabet:
    -- a stack is spelled through its letters, so the glyph prints as its
    -- full letter (the subjoined r and g of rgya print r and g). The raised
    -- glyphs keep the raised spelling they read back as, so that what a
    -- stack writes it also reads.
    TcSubConsonant SCW -> Just "W"
    TcSubConsonant SCY -> Just "Y"
    TcSubConsonant sc -> Just (wylieConsonant (fromSubjoined sc))
    TcVowel v -> Just (wylieVowel v)
    TcFinal f -> Just (wylieFinal f)
    TcNumber n -> Just (wylieNumber n)
    TcHalfNumber h -> Just (wylieHalfNumber h)
    TcPunctuation p -> Just (wyliePunctuation p)
    TcSign s -> Just (wylieSign s)
    TcSanskritMark m -> Just (wylieSanskritMark m)
    TcOrnament o -> Just (wylieOrnament o)
    TcSymbol s -> Just (wylieSymbol s)
    TcSpace m -> Just (wylieSpace m)
    TcConSpec cs -> Just (wylieConSpec cs)
    _ -> Nothing

-- | Wylie spelling of a ConSpec operator.
wylieConSpec :: ConSpec -> Text
wylieConSpec = \case
    CSPlus -> "+"
    CSDot -> "."

wylieConsonant :: Consonant -> Text
wylieConsonant = \case
    Ck -> "k"
    Ckh -> "kh"
    Cg -> "g"
    Cng -> "ng"
    Cc -> "c"
    Cch -> "ch"
    Cj -> "j"
    Cny -> "ny"
    CT -> "T"
    CTh -> "Th"
    CD -> "D"
    CN -> "N"
    Ct -> "t"
    Cth -> "th"
    Cd -> "d"
    Cn -> "n"
    Cp -> "p"
    Cph -> "ph"
    Cb -> "b"
    Cm -> "m"
    Cts -> "ts"
    Ctsh -> "tsh"
    Cdz -> "dz"
    Cw -> "w"
    Czh -> "zh"
    Cz -> "z"
    C' -> "'"
    Cy -> "y"
    Cr -> "r"
    Cl -> "l"
    Csh -> "sh"
    CSh -> "Sh"
    Cs -> "s"
    Ch -> "h"
    Ca -> "a"
    CR -> "R"
    Ckka -> "\\u0f6b"
    CRra -> "\\u0f6c"

wylieVowel :: Vowel -> Text
wylieVowel = \case
    VA -> "A"
    Vi -> "i"
    Vu -> "u"
    Ve -> "e"
    Vai -> "ai"
    Vo -> "o"
    Vau -> "au"
    V_i -> "-i"

wylieFinal :: FinalMark -> Text
wylieFinal = \case
    FMAnusvara -> "M"
    FMBinduNada -> "~M`"
    FMCandrabindu -> "~M"
    FMSrogMed -> "X"
    FMCandrabinduHalanta -> "~X"
    FMVisarga -> "H"
    FMHalanta -> "?"
    FMCaret -> "^"
    FMYigMgo -> "&"

wylieNumber :: Number -> Text
wylieNumber = \case
    N0 -> "0"
    N1 -> "1"
    N2 -> "2"
    N3 -> "3"
    N4 -> "4"
    N5 -> "5"
    N6 -> "6"
    N7 -> "7"
    N8 -> "8"
    N9 -> "9"

wylieHalfNumber :: HalfNumber -> Text
wylieHalfNumber = \case
    H_0 -> "\\u0f33"
    H_1 -> "\\u0f2a"
    H_2 -> "\\u0f2b"
    H_3 -> "\\u0f2c"
    H_4 -> "\\u0f2d"
    H_5 -> "\\u0f2e"
    H_6 -> "\\u0f2f"
    H_7 -> "\\u0f30"
    H_8 -> "\\u0f31"
    H_9 -> "\\u0f32"

wyliePunctuation :: PunctuationMark -> Text
wyliePunctuation = \case
    PMTsheg -> " "
    PMNonBreakingTsheg -> "*"
    PMShad -> "/"
    PMNyisShad -> "//"
    PMTshegShad -> ";"
    PMNyisTshegShad -> "\\u0f10"
    PMRinChenSpungsShad -> "|"
    PMRgyaGramShad -> "\\u0f12"
    PMCaretDzudRtagsMeLong -> "\\u0f13"
    PMGterTshigMgo -> ":"

wylieSign :: SignMark -> Text
wylieSign = \case
    SGKaKhaGaGsum -> "\\u0f01"
    SGNyiZlaNaaDa -> "\\u0f02"
    SGSbrulShad -> "\\u0f03"

wylieSanskritMark :: SanskritMark -> Text
wylieSanskritMark = \case
    SMiLciRtags -> "\\u0f86"
    SMiYangRtags -> "\\u0f87"
    SMiLceTsaCanSubjoined -> "\\u0f8d"
    SMiMchuCanSubjoined -> "\\u0f8e"
    SMiInvertedMchuCanSubjoined -> "\\u0f8f"

wylieOrnament :: OrnamentMark -> Text
wylieOrnament = \case
    OMRdelDkarGcig -> "\\u0fd0"
    OMRdelDkarGnyis -> "\\u0fd1"
    OMRdelDkarGsum -> "\\u0fd2"
    OMRdelNagGcig -> "\\u0fd3"
    OMRdelNagGnyis -> "\\u0fd4"
    OMLeadingMchanRtags -> "\\u0fd9"
    OMTrailingMchanRtags -> "\\u0fda"

wylieSymbol :: SymbolMark -> Text
wylieSymbol = \case
    SMExclamation -> "!"
    SMAt -> "@"
    SMHash -> "#"
    SMDollar -> "$"
    SMPercent -> "%"
    SMEqual -> "="
    SMLt -> "<"
    SMGt -> ">"
    SMLParen -> "("
    SMRParen -> ")"

wylieSpace :: SpaceMark -> Text
wylieSpace = \case
    SMSpace -> "_"

wylieConsonantAliases :: [(Text, Consonant)]
wylieConsonantAliases =
    [ ("-t", CT)
    , ("-th", CTh)
    , ("-d", CD)
    , ("-n", CN)
    , ("-sh", CSh)
    , ("\x2019", C') -- U+2019: the typographic apostrophe is the a-chung too
    , ("\x2018", C') -- U+2018: so is the left typographic quote ('ga' in ewts-js)
    ]

-- | EWTS raised spellings of the subjoined w and y. They are signs of their
-- own, not spellings of the full letters: the reference writes them for the
-- raised glyphs ྺ/ྻ (U+0FBA/0FBB), and a stack that read one prints it back
-- (REFACTOR-AST §10.3 Р3).
wylieRaisedSubjoined :: [(Text, SubConsonant)]
wylieRaisedSubjoined =
    [ ("W", SCW)
    , ("Y", SCY)
    ]

-- | Wylie spellings that expand to several canonical tokens.
--
-- These are the compound forms of the canonical domain: aspirated consonants
-- ("gh" = g + subjoined-h), long vowels ("I" = A+i, "E" = A+e, ...),
-- vocalic r/l ("r-i" = subjoined-r + reverse-i) and the EWTS letters
-- f/v ("f" = ph + caret, "v" = b + caret). No precomposed canonical
-- exists for them, so a single spelling produces a token sequence.
wylieExpansions :: [(Text, [TokenCanonical])]
wylieExpansions =
    [ ("gh", [TcConsonant Cg, TcSubConsonant SCh])
    , ("g+h", [TcConsonant Cg, TcSubConsonant SCh])
    , ("Dh", [TcConsonant CD, TcSubConsonant SCh])
    , ("D+h", [TcConsonant CD, TcSubConsonant SCh])
    , ("dh", [TcConsonant Cd, TcSubConsonant SCh])
    , ("d+h", [TcConsonant Cd, TcSubConsonant SCh])
    , ("bh", [TcConsonant Cb, TcSubConsonant SCh])
    , ("b+h", [TcConsonant Cb, TcSubConsonant SCh])
    , ("dzh", [TcConsonant Cdz, TcSubConsonant SCh])
    , ("dz+h", [TcConsonant Cdz, TcSubConsonant SCh])
    , ("k+Sh", [TcConsonant Ck, TcSubConsonant SCSh])
    , ("f", [TcConsonant Cph, TcFinal FMCaret])
    , ("v", [TcConsonant Cb, TcFinal FMCaret])
    , ("-dh", [TcSubConsonant SCD, TcSubConsonant SCh])
    , ("-d+h", [TcSubConsonant SCD, TcSubConsonant SCh])
    , ("I", [TcVowel VA, TcVowel Vi])
    , ("U", [TcVowel VA, TcVowel Vu])
    , ("E", [TcVowel VA, TcVowel Ve])
    , ("O", [TcVowel VA, TcVowel Vo])
    , ("-I", [TcVowel VA, TcVowel V_i])
    , ("r-i", [TcSubConsonant SCr, TcVowel V_i])
    , ("r-I", [TcSubConsonant SCr, TcVowel VA, TcVowel V_i])
    , ("l-i", [TcSubConsonant SCl, TcVowel V_i])
    , ("l-I", [TcSubConsonant SCl, TcVowel VA, TcVowel V_i])
    ]

inverseWylieConsonant :: Text -> Maybe Consonant
inverseWylieConsonant = inverseMap wylieConsonant

inverseWylieVowel :: Text -> Maybe Vowel
inverseWylieVowel = inverseMap wylieVowel

inverseWylieFinal :: Text -> Maybe FinalMark
inverseWylieFinal = inverseMap wylieFinal

inverseWylieNumber :: Text -> Maybe Number
inverseWylieNumber = inverseMap wylieNumber

inverseWyliePunctuation :: Text -> Maybe PunctuationMark
inverseWyliePunctuation = inverseMap wyliePunctuation

inverseWylieSymbol :: Text -> Maybe SymbolMark
inverseWylieSymbol = inverseMap wylieSymbol

inverseWylieSpace :: Text -> Maybe SpaceMark
inverseWylieSpace = inverseMap wylieSpace
