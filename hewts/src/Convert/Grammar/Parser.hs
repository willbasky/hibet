module Convert.Grammar.Parser where

import Convert.Token
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)

type Parser = Parsec Void [Token]

parseEither :: Parser a -> [Token] -> Either Text a
parseEither p ts =
    case runParser p "" ts of
        Left _ -> Left (T.pack "Token parser error")
        Right v -> Right v

recovering :: Parser a -> Parser a
recovering p = withRecovery (\e -> registerParseError e *> skipGarbage *> p) p

skipGarbage :: Parser ()
skipGarbage = skipMany (satisfy (not . isPunctuationLike)) <* optional pPunctuation

pToken :: Parser Token
pToken = anySingle

pConsonant :: Parser Token
pConsonant = satisfy isConsonantToken <?> "Consonant token"

pRootConsonant :: Parser Token
pRootConsonant = satisfy isRootConsonantToken <?> "One of 30 Tibetan root consonants"

pSanskrit :: Parser Token
pSanskrit = satisfy isSanskritConsonantToken <?> "One of 5 Sanskrit consonants"

-- | The four subfix letters. The sets are the book's (rules 4.5, 4.6: subfixes
-- are @w y r l@, superfixes @r l s@). Wylie writes the bare letter where
-- Tibetan has a separate sign, so each gets a spelling: the letter is the same
-- token either way, only its canonical form differs.
-- | Which spelling the input is written in. The two agree on almost everything
-- - @i u e o@ are the same letters either way, a prefix is a prefix, a final is
-- a final - and differ where Tibetan lets a mark carry the meaning and Wylie
-- writes it out in letters: a bare @r y l w@ is the subjoined letter in Wylie
-- and a separate sign in Tibetan. So the switch changes which tokens fill a
-- slot, never what the slot means.
data Spelling
    = Tibetan
    | Wylie
    deriving (Show, Eq, Ord)

pSubfixWa :: Parser Token
pSubfixWa = satisfy (isSpecificSubConsonant SCw) <?> "Subfix wa token"

pSubfixYa :: Parser Token
pSubfixYa = satisfy (isSpecificSubConsonant SCy) <?> "Subfix ya token"

pSubfixRa :: Parser Token
pSubfixRa = satisfy (isSpecificSubConsonant SCr) <?> "Subfix ra token"

pSubfixLa :: Parser Token
pSubfixLa = satisfy (isSpecificSubConsonant SCl) <?> "Subfix la token"

-- | Wylie writes the subfix letters with their full letters (@rky@ -> ཀྱ);
-- the subjoined signs exist only in the Tibetan spelling, so each subfix slot
-- has a Wylie twin that matches the plain letter.
pSubfixYaWylie :: Parser Token
pSubfixYaWylie = satisfy (isSpecificConsonant Cy) <?> "Wylie subfix ya letter"

pSubfixRaWylie :: Parser Token
pSubfixRaWylie = satisfy (isSpecificConsonant Cr) <?> "Wylie subfix ra letter"

pSubfixLaWylie :: Parser Token
pSubfixLaWylie = satisfy (isSpecificConsonant Cl) <?> "Wylie subfix la letter"

pSubfixWaWylie :: Parser Token
pSubfixWaWylie = satisfy (isSpecificConsonant Cw) <?> "Wylie subfix wa letter"

pVowel :: Parser Token
pVowel = satisfy isVowelToken <?> "Vowel token"

pVowelLongA :: Parser Token
pVowelLongA = satisfy isLongAVowelToken <?> "Long vowel token"

-- | The letter @a@ where a vowel would be: written in every Wylie syllable
-- and never printed. The Wylie arms of the structures and constraints that
-- assemble their own vowel slot reach for it when the spelling has no vowel
-- sign.
pImplicitA :: Parser Token
pImplicitA = satisfy isImplicitA

isImplicitA :: Token -> Bool
isImplicitA Token{tokenCanonical = TcConsonant Ca} = True
isImplicitA _ = False

pPrefixGa :: Parser Token
pPrefixGa = satisfy (isSpecificConsonant Cg) <?> "Prefix ga token"

pPrefixDa :: Parser Token
pPrefixDa = satisfy (isSpecificConsonant Cd) <?> "Prefix da token"

pPrefixBa :: Parser Token
pPrefixBa = satisfy (isSpecificConsonant Cb) <?> "Prefix ba token"

pPrefixMa :: Parser Token
pPrefixMa = satisfy (isSpecificConsonant Cm) <?> "Prefix ma token"

pPrefixA :: Parser Token
pPrefixA = satisfy (isSpecificConsonant C') <?> "Prefix a-chung token"

pSuperfixRa :: Parser Token
pSuperfixRa = satisfy (isSpecificConsonant Cr) <?> "Superfix ra token"

pSuperfixLa :: Parser Token
pSuperfixLa = satisfy (isSpecificConsonant Cl) <?> "Superfix la token"

pSuperfixSa :: Parser Token
pSuperfixSa = satisfy (isSpecificConsonant Cs) <?> "Superfix sa token"

pSubConsonant :: Parser Token
pSubConsonant = satisfy isSubConsonantToken <?> "Subconsonant token"

pSuffix :: Parser Token
pSuffix = satisfy isSuffixConsonantToken <?> "Suffix token"

pPostfix :: Parser Token
pPostfix = satisfy isPostfixConsonantToken <?> "Postfix token"

pPostfixDa :: Parser Token
pPostfixDa = satisfy (isSpecificConsonant Cd) <?> "Postfix da token"

pPostfixSa :: Parser Token
pPostfixSa = satisfy (isSpecificConsonant Cs) <?> "Postfix sa token"

pFinal :: Parser Token
pFinal = satisfy isFinalToken <?> "Final mark token"

pPunctuation :: Parser Token
pPunctuation = satisfy isPunctuationLike <?> "Punctuation-like token"

pNumber :: Parser Token
pNumber = satisfy isNumberToken <?> "Number token"

pHalfNumber :: Parser Token
pHalfNumber = satisfy isHalfNumberToken <?> "Half number token"

pSign :: Parser Token
pSign = satisfy isSignToken <?> "Sign token"

pSanskritMark :: Parser Token
pSanskritMark = satisfy isSanskritMarkToken <?> "Sanskrit mark token"

pOrnament :: Parser Token
pOrnament = satisfy isOrnamentToken <?> "Ornament token"

pSpace :: Parser Token
pSpace = satisfy isSpaceToken <?> "Space token"

pSymbol :: Parser Token
pSymbol = satisfy isSymbolToken <?> "Symbol token"

pUnknown :: Parser Token
pUnknown = satisfy isUnknownToken <?> "Unknown token"

isConsonantToken :: Token -> Bool
isConsonantToken Token{tokenCanonical = TcConsonant _} = True
isConsonantToken _ = False

isRootConsonantToken :: Token -> Bool
isRootConsonantToken Token{tokenCanonical = TcConsonant c} = c `elem` rootConsonants
isRootConsonantToken _ = False

isSanskritConsonantToken :: Token -> Bool
isSanskritConsonantToken Token{tokenCanonical = TcConsonant c} = c `elem` sanskritConsonants
isSanskritConsonantToken _ = False

isVowelToken :: Token -> Bool
isVowelToken Token{tokenCanonical = TcVowel v} = v `elem` shortVowels
isVowelToken _ = False

shortVowels :: [Vowel]
shortVowels = [Vi, Ve, Vo, Vu]

isLongAVowelToken :: Token -> Bool
isLongAVowelToken Token{tokenCanonical = TcVowel VA} = True
isLongAVowelToken _ = False

isSubConsonantToken :: Token -> Bool
isSubConsonantToken Token{tokenCanonical = TcSubConsonant _} = True
isSubConsonantToken _ = False

isSuffixConsonantToken :: Token -> Bool
isSuffixConsonantToken Token{tokenCanonical = TcConsonant c} = c `elem` suffixConsonants
isSuffixConsonantToken _ = False

isPostfixConsonantToken :: Token -> Bool
isPostfixConsonantToken Token{tokenCanonical = TcConsonant c} = c `elem` postfixConsonants
isPostfixConsonantToken _ = False

isFinalToken :: Token -> Bool
isFinalToken Token{tokenCanonical = TcFinal _} = True
isFinalToken _ = False

isPunctuationLike :: Token -> Bool
isPunctuationLike t = isPunctuationToken t || isSpaceToken t

isPunctuationToken :: Token -> Bool
isPunctuationToken Token{tokenCanonical = TcPunctuation _} = True
isPunctuationToken _ = False

isNumberToken :: Token -> Bool
isNumberToken Token{tokenCanonical = TcNumber _} = True
isNumberToken _ = False

isHalfNumberToken :: Token -> Bool
isHalfNumberToken Token{tokenCanonical = TcHalfNumber _} = True
isHalfNumberToken _ = False

isSignToken :: Token -> Bool
isSignToken Token{tokenCanonical = TcSign _} = True
isSignToken _ = False

isSanskritMarkToken :: Token -> Bool
isSanskritMarkToken Token{tokenCanonical = TcSanskritMark _} = True
isSanskritMarkToken _ = False

isOrnamentToken :: Token -> Bool
isOrnamentToken Token{tokenCanonical = TcOrnament _} = True
isOrnamentToken _ = False

isSpaceToken :: Token -> Bool
isSpaceToken Token{tokenCanonical = TcSpace _} = True
isSpaceToken _ = False

isSymbolToken :: Token -> Bool
isSymbolToken Token{tokenCanonical = TcSymbol _} = True
isSymbolToken _ = False

isUnknownToken :: Token -> Bool
isUnknownToken Token{tokenCanonical = TcUnknown _} = True
isUnknownToken _ = False

isSpecificConsonant :: Consonant -> Token -> Bool
isSpecificConsonant c Token{tokenCanonical = TcConsonant c'} = c == c'
isSpecificConsonant _ _ = False

isSpecificSubConsonant :: SubConsonant -> Token -> Bool
isSpecificSubConsonant c Token{tokenCanonical = TcSubConsonant c'} = c == c'
isSpecificSubConsonant _ _ = False

rootConsonants :: [Consonant]
rootConsonants =
    [ Ck
    , Ckh
    , Cg
    , Cng
    , Cc
    , Cch
    , Cj
    , Cny
    , Ct
    , Cth
    , Cd
    , Cn
    , Cp
    , Cph
    , Cb
    , Cm
    , Cts
    , Ctsh
    , Cdz
    , Cw
    , Czh
    , Cz
    , C'
    , Cy
    , Cr
    , Cl
    , Csh
    , Cs
    , Ch
    , Ca
    ]

sanskritConsonants :: [Consonant]
sanskritConsonants = [CT, CTh, CD, CN, CSh]

suffixConsonants :: [Consonant]
suffixConsonants = [C', Cg, Cng, Cd, Cn, Cb, Cm, Cr, Cl, Cs]

postfixConsonants :: [Consonant]
postfixConsonants = [Cd, Cs]
