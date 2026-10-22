module Convert.Grammar.Parser where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import Convert.Diagnostic (Finding (..))
import Convert.Grammar.Syllable
    ( Stack
    , TibetanSyllable
    , closeTop
    , putFinal
    , putImplicitVowel
    , putPostfix
    , putPrefix
    , putRoot
    , putSubfix
    , putSuffix
    , putSuperfix
    , putVowel
    )
import Convert.Token
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as MP

type Parser = Parsec Void [Token]

-- | The spelling state that lives in the grammar parsers: the stacks the run
-- has built so far ('scanStacks'), the findings the constraint windows
-- recorded for the syllable run being parsed, plus the context bits the rules
-- of wave 3.4.4 thread across the stacks of one word - the bare prefix or
-- superfix the word opened on, still waiting for the stack it leads
-- ('scanLeading'), the consonant positions of the word tail after its last
-- vowel ('scanTail'), and how far the stack being spelled has filled the chain
-- of its final marks ('scanFinalSlot', 'scanCaret'). A new wave-3.4 rule adds a
-- constructor to 'Finding' and a window; these fields are the only context the
-- windows of a run share.
data ScanState = ScanState
    { scanStacks :: Seq Stack
    , scanFindings :: [Finding]
    , scanLeading :: Maybe (LeadRole, Token)
    , scanTail :: WordTail
    , scanFinalSlot :: Int
    , scanCaret :: Bool
    }

-- | The head letter a word opened on and that still waits for the stack it
-- leads: a superfix that must gate a root with a vowel, or a prefix whose
-- root stack has to reach a vowel.
data LeadRole
    = LeadSuperfix
    | LeadPrefix
    deriving (Show, Eq)

-- | The consonant positions of the word tail after its last vowel: no vowel
-- placed yet ('TailVoid'), or the first and the second suffix slot standing
-- after it. The second holds only the one postfix pair rule 4.16 allows; a
-- third consonant can follow none of them.
data WordTail
    = TailVoid
    | TailOpen (Maybe Token) (Maybe Token)
    deriving (Show, Eq)

-- | The spell parser: the grammar runs in 'StateT' over the token parser, so
-- the state lives in the parsers themselves: the stacks built so far, the
-- findings the windows recorded and the word context. Megaparsec's
-- @MonadParsec@ instances for 'StateT' (its own README: wrap @ParsecT@ in these
-- monads to add backtracking state) give us the usual combinators: @<|>@ runs
-- the second arm from the state before the first, so a failed arm takes its
-- tokens and its stacks with it. @lookAhead@ does not roll that state back (it
-- is carried in the value, not in the parser's own state), so a probe runs the
-- real parse in a projection that discards the state ('runSpell').
type SpellParser = StateT ScanState Parser

initialScanState :: ScanState
initialScanState =
    ScanState
        { scanStacks = mempty
        , scanFindings = []
        , scanLeading = Nothing
        , scanTail = TailVoid
        , scanFinalSlot = 0
        , scanCaret = False
        }

-- | Run a stateful spelling parse as a plain one, discarding the state: the
-- entry point of the sentence runner and the pure projection of every probe.
runSpell :: SpellParser a -> Parser a
runSpell = flip evalStateT initialScanState

-- | Lift a plain token parser into the spell parser.
liftP :: Parser a -> SpellParser a
liftP = lift

-- | Give a token its slot: it is read, put into the stack being built (the
-- grammar's own state) and returned as part of the run. Placing at the point of
-- binding keeps the token parsers themselves unchanged: they parse letters, they
-- know nothing about syllables.
--
-- The state is the one truth about stacks: the same call decides where the
-- letter stands, so the parse needs no grouping pass over the run. A failed arm
-- is rolled back by '<|>' together with its tokens, so a probe or an alternative
-- leaves no half-built stack behind. One function per slot of 'Stack'.
markPrefix
    , markSuffix
    , markPostfix
    , markSuperfix
    , markRoot
    , markSubfix
    , markVowel
    , markImplicitVowel
    , markFinal ::
        Parser Token -> SpellParser TibetanSyllable
markPrefix = markAt putPrefix
markSuffix = markAt putSuffix
markPostfix = markAt putPostfix
markSuperfix = markAt putSuperfix
markRoot = markAt putRoot
markSubfix = markAt putSubfix
markVowel = markAt putVowel
markImplicitVowel = markAt putImplicitVowel
markFinal = markAt putFinal

-- | The same for a token already read: the structures that build their slot from
-- a token in hand.
placePrefix
    , placeSuffix
    , placePostfix
    , placeSuperfix
    , placeRoot
    , placeSubfix
    , placeVowel
    , placeFinal ::
        Token -> SpellParser TibetanSyllable
placePrefix = placeAt putPrefix
placeSuffix = placeAt putSuffix
placePostfix = placeAt putPostfix
placeSuperfix = placeAt putSuperfix
placeRoot = placeAt putRoot
placeSubfix = placeAt putSubfix
placeVowel = placeAt putVowel
placeFinal = placeAt putFinal

markAt ::
    (Token -> Seq Stack -> Seq Stack) -> Parser Token -> SpellParser TibetanSyllable
markAt put parser = placeAt put =<< liftP parser

placeAt ::
    (Token -> Seq Stack -> Seq Stack) -> Token -> SpellParser TibetanSyllable
placeAt put tok = do
    modify $ \s -> s{scanStacks = put tok (scanStacks s)}
    pure (Seq.singleton tok)

-- | The subfix letters of one stack, placed in order.
subfixMarks :: [Token] -> SpellParser TibetanSyllable
subfixMarks = fmap mconcat . mapM placeSubfix

-- | The one caret that survives a subjoining scan, or nothing when the scan
-- kept no caret.
caretMark :: Maybe Token -> SpellParser TibetanSyllable
caretMark = maybe (pure mempty) placeFinal

-- | The stacks the run's grammar built, closed, and the state is cleared for
-- the next run. Every token a structure places is already in the stack that
-- owns it, so the caller reads the syllable instead of grouping the marks.
takeStacks :: SpellParser (Seq Stack)
takeStacks = do
    stacks <- gets scanStacks
    modify $ \s -> s{scanStacks = mempty}
    pure (closeTop stacks)

-- | Record one finding of a constraint window in the running state, before
-- the run's span is known.
noteFinding :: Finding -> SpellParser ()
noteFinding f = modify $ \s -> s{scanFindings = f : scanFindings s}

-- | The findings the current run's windows recorded, without clearing them:
-- for a window that wants to know whether a sibling already warned.
peekFindings :: SpellParser [Finding]
peekFindings = gets scanFindings

-- | The stacks the run has built so far, read without clearing them.
peekStacks :: SpellParser (Seq Stack)
peekStacks = gets scanStacks

-- | The findings the current run's windows recorded, in order; the state is
-- cleared for the next run ('pSyllable' collects the ones of the run it
-- finished). The leading-letter and word-tail context belongs to the run
-- too: no bare prefix or superfix survives into the next word.
takeFindings :: SpellParser [Finding]
takeFindings = do
    fs <- gets scanFindings
    modify $ \s ->
        s
            { scanFindings = []
            , scanLeading = Nothing
            , scanTail = TailVoid
            }
    pure (reverse fs)

-- | A new stack begins: the chain of its final marks is empty again. A
-- syllable's finals belong to the stack they close, not to the whole run, so
-- @kH gaM@ is two legal stacks - read as one chain it would put the @M@ behind
-- a slot the visarga of the first stack had passed.
resetFinalChain :: SpellParser ()
resetFinalChain = modify $ \s -> s{scanFinalSlot = 0, scanCaret = False}

-- | How a final mark stood against the chain its stack's earlier finals built.
data FinalFit
    = -- | The mark filled its slot, or was the stack's first caret.
      FinalFits
    | -- | A sign mark that came twice or came back to front: the chain's own
      -- window, and the reason 'Convert.Token.finalSlot' numbers the slots.
      FinalOutOfChain
    | -- | A second caret. The caret fills no slot - it is transparent while
      -- the subjoining run goes on and prints below it, not after the chain -
      -- so it has a rule of its own and its own wording.
      FinalRepeatedCaret

-- | Take one final mark's place in the chain and say how it stood: a sign mark
-- fills the next slot only while the chain has not passed it
-- (@Convert.Token.finalSlot@), and a caret only has to be the stack's first.
-- A mark that does not fit is the window's own finding; the chain does not walk
-- back for it, because the mark is dropped, not moved.
claimFinal :: Token -> SpellParser FinalFit
claimFinal tok = case tokenCanonical tok of
    TcFinal fm -> case finalSlot fm of
        Nothing -> do
            seen <- gets scanCaret
            modify $ \s -> s{scanCaret = True}
            pure (if seen then FinalRepeatedCaret else FinalFits)
        Just slot -> do
            high <- gets scanFinalSlot
            modify $ \s -> s{scanFinalSlot = max high slot}
            pure (if slot > high then FinalFits else FinalOutOfChain)
    _ -> pure FinalFits

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

-- | Whether the token is written in Wylie. The sign decides the spelling, not a
-- label: a Wylie token is a plain letter where Tibetan has a joined sign.
isWylieSign :: Token -> Bool
isWylieSign t = tokenSource t == TsWylie

-- | A letter the book writes as a joined sign in Tibetan and as the full letter
-- in Wylie (rules 4.5, 4.6: subfixes are @w y r l@). The sign decides which form
-- it may take; the other form is not this letter.
isJoinedAt :: SubConsonant -> Consonant -> Token -> Bool
isJoinedAt sc c t
    | isWylieSign t = isSpecificConsonant c t
    | otherwise = isSpecificSubConsonant sc t

pSubfixWa :: Parser Token
pSubfixWa = satisfy (isJoinedAt SCw Cw) <?> "Subfix wa token"

pSubfixYa :: Parser Token
pSubfixYa = satisfy (isJoinedAt SCy Cy) <?> "Subfix ya token"

pSubfixRa :: Parser Token
pSubfixRa = satisfy (isJoinedAt SCr Cr) <?> "Subfix ra token"

pSubfixLa :: Parser Token
pSubfixLa = satisfy (isJoinedAt SCl Cl) <?> "Subfix la token"

-- | A subjoined root from a group of letters: the joined sign in Tibetan, the
-- full letter in Wylie.
pJoinedRoot :: [Consonant] -> Parser Token
pJoinedRoot allowed =
    satisfy (maybe False (`elem` allowed) . letterOfSign) <?> "Subjoined root token"

letterOfSign :: Token -> Maybe Consonant
letterOfSign t = case (isWylieSign t, tokenCanonical t) of
    (True, TcConsonant c) -> Just c
    (False, TcSubConsonant sc) -> Just (fromSubjoined sc)
    _ -> Nothing

-- | Whether the stack written so far is Wylie: its letters carry the Wylie source.
isWylieStack :: TibetanSyllable -> Bool
isWylieStack = any isWylieSign

-- | The vowel slot after a stack. Wylie writes a vowel in every syllable (a
-- vowel letter, or the letter @a@ where none stands), so its slot is
-- obligatory; Tibetan may leave it out. @written@ are the vowel letters the rule
-- allows.
vowelSlot ::
    TibetanSyllable -> [SpellParser TibetanSyllable] -> SpellParser TibetanSyllable
vowelSlot stack written
    | isWylieStack stack = MP.choice (written ++ [markImplicitVowel pImplicitA])
    | otherwise = fromMaybe mempty <$> MP.optional (MP.choice written)

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

-- | The same letter, but only as the Wylie spelling writes it. A Tibetan
-- a-chen (0x0f68) is a written letter that opens a stack of its own - the
-- vowel sign behind it still belongs to that stack (ཨེ is one stack, the
-- Wylie @ae@ two) - so the generic stack takes @a@ as its unprinted implicit
-- vowel by this narrower predicate, and a written a-chen as a base like any
-- other consonant.
isImplicitAWylie :: Token -> Bool
isImplicitAWylie t = isImplicitA t && tokenSource t == TsWylie

pImplicitAWylie :: Parser Token
pImplicitAWylie = satisfy isImplicitAWylie

-- | A vowel the generic stack can absorb. Wylie writes the long letters as
-- their two short parts (@AH@, @mA@, @oM@), so every vowel token is absorbable
-- there; the Tibetan side takes the short vowels, the compound signs @ཻ@ @ཽ@ @ྀ@
-- (0x0f7b, 0x0f7d, 0x0f80) that bind a stack just as the short ones do, and the
-- long @a@ sign @ཱ@ (0x0f71). So «ཨ» + «ཻ» is one stack (@ai@, not an a-chen
-- with a vowel left over), and «ཱ» opening a stack leaves the slot for the
-- short vowel that completes the long letter («ཱ» + «ི» reads back as @I@).
-- A lone «ཨཱ» is still the book's long-a window's: structure 1 claims it on the
-- length tie, so the sign standing by itself keeps its own shape. The lattice
-- is decided per token, by 'tokenSource', not by the 'Spelling' label: the
-- parity runs Wylie token lists under 'Tibetan' as well.
pVowelAny :: Parser Token
pVowelAny = satisfy isEatableVowel

-- | The bare short vowel that completes a long vowel, seen from the long @a@
-- token that opened the slot. Wylie spells the long letters as two letters, and
-- the second token carries no text of its own (its raw is empty) because the
-- one letter wrote both (@I@ -> @A@ + @i@, @-I@ -> @A@ + @-i@); a short vowel
-- written out on its own (@Ai@ as @A@ then a spelled @i@, @kAi@ as @k@ + @A@ +
-- @i@) keeps its raw text and opens a stack of its own. Tibetan spells the long
-- letter with the sign @ཱ@ (0x0f71) and the short sign that completes it, each
-- its own codepoint, so there the predicate is the completing short vowel
-- itself - @ི@ @ུ@ @ྀ@ combine into @I@ @U@ @-I@ (@ཱ@ + @ེ@ stays two, @A@ then
-- @e@, as the reference reads it).
pLongVowelHalf :: Parser Token
pLongVowelHalf = satisfy isLongVowelHalf

isLongVowelHalf :: Token -> Bool
isLongVowelHalf t@Token{tokenCanonical = TcVowel v}
    | tokenSource t == TsUnicode = v `elem` [Vi, Vu, V_i]
    | otherwise = T.null (tokenRaw t)
isLongVowelHalf _ = False

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
-- Returns the chosen letters and every caret the run passed - the window
-- that keeps the first (it prints below the run) counts the rest and blames
-- a second one. Stops at the first token that is neither, leaving it in
-- place.
pSubjoinRun :: Parser ([Token], [Token])
pSubjoinRun = continueRun [] []
    where
        -- One more piece of the run: a caret, a letter, or its end.
        continueRun :: [Token] -> [Token] -> Parser ([Token], [Token])
        continueRun subjoined carets =
            MP.choice
                [ swallowCaretInRun subjoined carets
                , takeLetterInRun subjoined carets
                , pure (subjoined, carets)
                ]

        -- A caret between the subjoined letters is transparent; the list of
        -- the run's carets keeps every one of them for the window.
        swallowCaretInRun :: [Token] -> [Token] -> Parser ([Token], [Token])
        swallowCaretInRun subjoined carets = MP.try $ do
            t <- pCaret
            continueRun subjoined (carets <> [t])

        -- One more bare letter below the base, when the run has room for it.
        takeLetterInRun :: [Token] -> [Token] -> Parser ([Token], [Token])
        takeLetterInRun subjoined carets = MP.try $ do
            t <- MP.satisfy isSubjoinCandidate
            if fitsBelow subjoined t
                then continueRun (subjoined <> [t]) carets
                else MP.empty

        -- A stack carries at most two subjoined letters, and @l@ never fills
        -- the second slot; without room the run simply ends here.
        fitsBelow :: [Token] -> Token -> Bool
        fitsBelow subjoined next =
            length subjoined < 2 && not (length subjoined == 1 && isL next)

-- | The tail of a syllable run: everything the winning structure left of the
-- word, up to the boundary. Which tokens the run swallows is the caller's
-- business - the sentence reads its own boundaries - while what a swallowed
-- stack dot means is the grammar's, and this is that window.
--
-- A dot joins a stack to the letter behind it, and the grammar's own dot
-- window ('Convert.Grammar.Constraint.Constraint21.pDotBreak') takes a dot only
-- when a consonant follows it, and marks that consonant. A dot that reaches
-- this window therefore joins nothing, and the first one is the run's single
-- 'UnplacedDot' finding; the rest of the tail is taken in silence, as one
-- finding per run is the rule.
pUnclaimedTail :: (Token -> Bool) -> SpellParser [Token]
pUnclaimedTail isTail = go
    where
        go :: SpellParser [Token]
        go = do
            -- The token is taken, not just looked at: the recursion below reads
            -- the run again, and a lookAhead would hand it the same token.
            tok <- MP.optional (MP.satisfy isTail)
            case tok of
                Nothing -> pure []
                Just t
                    | isDot t -> do
                        noteFinding UnplacedDot
                        (t :) <$> MP.many (MP.satisfy isTail)
                    | otherwise -> (t :) <$> go

-- | Whether a vowel token belongs to a stack (see 'pVowelAny').
isEatableVowel :: Token -> Bool
isEatableVowel tok@Token{tokenCanonical = TcVowel v}
    | tokenSource tok == TsUnicode = v `elem` [Vi, Ve, Vo, Vu, Vai, Vau, V_i, VA]
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
