module Convert.Grammar.Parser where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import Convert.Diagnostic (Finding)
import Convert.Token
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as MP

type Parser = Parsec Void [Token]

-- | The spelling state that lives in the grammar parsers: the findings the
-- constraint windows recorded for the syllable run being parsed. The bare
-- finding list is all the state carries - every rule's window in every
-- constraint writes through 'noteFinding' and the run's edge collects them
-- with 'takeFindings', so a new wave-3.4 rule adds a constructor to
-- 'Finding' and a window, never a field here.
data ScanState = ScanState
    { scanFindings :: [Finding]
    }

-- | The spell parser: the grammar runs in 'StateT' over the token parser, so
-- the state lives in the parsers themselves. Megaparsec's @MonadParsec@
-- instances for 'StateT' (its own README: wrap @ParsecT@ in these monads to
-- add backtracking state) give us the usual combinators, with its semantics:
-- @lookAhead@ resets the state, @try@ does not and @<|>@ runs the second arm
-- from the state before the first. Probes run their parsers in a pure
-- projection ('runSpell'), so only the real run of the winning structure
-- writes state.
type SpellParser = StateT ScanState Parser

initialScanState :: ScanState
initialScanState = ScanState{scanFindings = []}

-- | Run a stateful spelling parse as a plain one, discarding the state: the
-- entry point of the sentence runner and the pure projection of every probe.
runSpell :: SpellParser a -> Parser a
runSpell = flip evalStateT initialScanState

-- | Lift a plain token parser into the spell parser.
liftP :: Parser a -> SpellParser a
liftP = lift

-- | Record one finding of a constraint window in the running state, before
-- the run's span is known.
noteFinding :: Finding -> SpellParser ()
noteFinding f = modify $ \s -> s{scanFindings = f : scanFindings s}

-- | The findings the current run's windows recorded, without clearing them:
-- for a window that wants to know whether a sibling already warned.
peekFindings :: SpellParser [Finding]
peekFindings = gets scanFindings

-- | The findings the current run's windows recorded, in order; the state is
-- cleared for the next run ('pSyllable' collects the ones of the run it
-- finished).
takeFindings :: SpellParser [Finding]
takeFindings = do
    fs <- gets scanFindings
    modify $ \s -> s{scanFindings = []}
    pure (reverse fs)

parseEither :: Parser a -> [Token] -> Either Text a
parseEither p ts =
    case runParser p "" ts of
        Left _ -> Left (T.pack "Token parser error")
        Right v -> Right v

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

-- | A vowel the generic stack can absorb. Wylie writes the long letters as
-- their two short parts (@AH@, @mA@, @oM@), so every vowel token is absorbable
-- there; Tibetan spells a long a explicitly (0x0f71) and a root written next
-- to it must not swallow it into the same word, so the Tibetan side takes only
-- the short vowels. The lattice is decided per token, by 'tokenSource', not by
-- the 'Spelling' label: the parity runs Wylie token lists under 'Tibetan' as
-- well.
pVowelAny :: Parser Token
pVowelAny = satisfy isEatableVowel

-- | The forced subjoin sign @+@: the letter after it is pushed below the base.
pPlus :: Parser Token
pPlus = satisfy isPlus

-- | The stack-breaking dot: ends a stack (@g.yon@ -> གཡོན).
pDot :: Parser Token
pDot = satisfy isDot

-- | The caret @^@ (0x0f39): a final mark that prints between the subfixes and
-- the vowel.
pCaret :: Parser Token
pCaret = satisfy isCaretLike

-- | The subjoining run below a base: the letters @{y, w, r, l}@ (bare in
-- Wylie, already-joined signs in Tibetan), at most two with @l@ never second,
-- and the carets in between, which are transparent while the scan goes on.
-- Returns the chosen letters and the one caret that survives; stops at the
-- first token that is neither, leaving it in place.
pSubjoinRun :: Parser ([Token], Maybe Token)
pSubjoinRun = continueRun [] Nothing
    where
        -- One more piece of the run: a caret, a letter, or its end.
        continueRun :: [Token] -> Maybe Token -> Parser ([Token], Maybe Token)
        continueRun subjoined caret =
            MP.choice
                [ swallowCaretInRun subjoined caret
                , takeLetterInRun subjoined caret
                , pure (subjoined, caret)
                ]

        -- A caret between the subjoined letters is transparent, but the first
        -- one is kept and prints below the run.
        swallowCaretInRun :: [Token] -> Maybe Token -> Parser ([Token], Maybe Token)
        swallowCaretInRun subjoined caret = MP.try $ do
            t <- pCaret
            continueRun subjoined (keepFirstCaret caret t)

        -- One more bare letter below the base, when the run has room for it.
        takeLetterInRun :: [Token] -> Maybe Token -> Parser ([Token], Maybe Token)
        takeLetterInRun subjoined caret = MP.try $ do
            t <- MP.satisfy isSubjoinCandidate
            if fitsBelow subjoined t
                then continueRun (subjoined <> [t]) caret
                else MP.empty

        -- A stack carries at most two subjoined letters, and @l@ never fills
        -- the second slot; without room the run simply ends here.
        fitsBelow :: [Token] -> Token -> Bool
        fitsBelow subjoined next =
            length subjoined < 2 && not (length subjoined == 1 && isL next)

        -- Only the first caret of the run prints.
        keepFirstCaret :: Maybe Token -> Token -> Maybe Token
        keepFirstCaret Nothing caret = Just caret
        keepFirstCaret kept _ = kept

-- | Whether a vowel token belongs to a stack (see 'pVowelAny').
isEatableVowel :: Token -> Bool
isEatableVowel tok@Token{tokenCanonical = TcVowel v}
    | tokenSource tok == TsUnicode = v `elem` [Vi, Ve, Vo, Vu]
    | otherwise = True
isEatableVowel _ = False

-- | A vowel-shaped token: an eatable vowel or the letter @a@ where a vowel
-- would be.
isVowelLike :: Token -> Bool
isVowelLike tok = isEatableVowel tok || isImplicitA tok

-- | The caret sign, wherever the scan of a stack meets it.
isCaretLike :: Token -> Bool
isCaretLike Token{tokenCanonical = TcFinal FMCaret} = True
isCaretLike _ = False

isPlus :: Token -> Bool
isPlus Token{tokenCanonical = TcConSpec CSPlus} = True
isPlus _ = False

isDot :: Token -> Bool
isDot Token{tokenCanonical = TcConSpec CSDot} = True
isDot _ = False

-- | The subjoining letter @l@, however it is spelled: bare in Wylie, a
-- subconsonant sign in Tibetan. It never sits below two consonants (grla is
-- ག + ར + ླ, not གྲླ).
isL :: Token -> Bool
isL Token{tokenCanonical = TcConsonant Cl} = True
isL Token{tokenCanonical = TcSubConsonant SCl} = True
isL _ = False

-- | A letter that can sit below another one: the bare @{y, w, r, l}@ a Wylie
-- writer spells in full letters, or a subconsonant token Tibetan spells
-- already joined. A Tibetan bare letter is a real letter and never a subjoin.
isSubjoinCandidate :: Token -> Bool
isSubjoinCandidate tok@Token{tokenCanonical = TcConsonant c}
    | tokenSource tok == TsWylie = c `elem` [Cl, Cr, Cw, Cy]
    | otherwise = False
isSubjoinCandidate Token{tokenCanonical = TcSubConsonant _} = True
isSubjoinCandidate _ = False

isPrefixLetter :: Token -> Bool
isPrefixLetter Token{tokenCanonical = TcConsonant c} = c `elem` [C', Cb, Cd, Cg, Cm]
isPrefixLetter _ = False

isSuperfixLetter :: Token -> Bool
isSuperfixLetter Token{tokenCanonical = TcConsonant c} = c `elem` [Cl, Cr, Cs]
isSuperfixLetter _ = False

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
