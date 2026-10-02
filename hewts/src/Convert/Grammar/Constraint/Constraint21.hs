{- Tibetan spelling grammar 4.21 (token parser variant)

The whole generic word of 'Convert.Grammar.Structure.pStructure38' - whatever
none of the book's 37 structures spell. One stack is the base consonant, the
letters subjoined to it, and the vowel that ends it; when a stack cannot reach
a vowel it keeps only its first consonant and everything after it begins a
fresh stack (jsewts's own backtrack). The cursor (caret, @^@) is transparent
while the stack subjoins and prints as a final mark between the subfixes and
the vowel; the forced-join sign @+@ (Wylie syntax only) joins the letter after
it below the stack; a word may open (ug -> ཨུག) and continue (g.yon -> གཡོན)
on anything outside the stack letters. A @+@ the stack already closed still
reads as a join: the letter after it stays in the same word (u+e -> ཨེུ,
rH+e -> རཿེ), as the references read it.

No spelling switch: the lattice - which bare letters subjoin, which vowels a
stack absorbs - is decided per token, by 'tokenSource', not by the 'Spelling'
of the parse. The parity runs Wylie token lists under 'Tibetan' as well, and
there is nowhere a @+@ or a dot could be spelled from a Tibetan stream, so the
same parser covers both arms, exactly as the old 'Convert.Grammar.Stack'
ignored its own 'Spelling' argument.

The head window (3.4.3) lives here, in the word lead: when the first stack
closes bare - the grammar placed a head letter that reached no vowel, no
subjoining run and no forced join - the head sits in the prefix position
unless it is a superfix letter gating a root ('markBase' says so; @rka@ and
@sgra@ hold a stack, never a prefix). The two prefix warnings of section 4.2
fire inline in that window, from the grammar's own predicates - the classic
§4.2 sets as the pair predicate 'prefixAllows' - and the finding is
spelled into a 'Diagnostic' by 'Convert.Sentence' when the run's span is
known. The reference machinery they replace (its stack scanner over the raw
run and its tables) is not carried over: the grammar itself decided the head
role; the walk reads no tokens after the parse ended.

The wave-3.4.4 legality windows (4.8-4.21) sit on the generic stack here, in
the moment a token is placed: the second caret of a run, two finals of one
class in a stack's tail, a forced @+@ that drags a consonant below a stack
whose vowel is already placed, a superfix that gates a root or a subfix
combination outside its own tables, a prefix whose stack never reaches a
vowel, and the 2nd-suffix pair rule (4.16) of the word tail. The context the
windows need - the pending head letter and the word's suffix positions -
lives in 'ScanState', not in tables over the finished run; each window is an
inline check that fires exactly where the grammar places the token.
-}

module Convert.Grammar.Constraint.Constraint21
    ( pConstraint21First
    , pConstraint21Rest
    ) where

import Control.Monad (void, when)
import Control.Monad.Trans.State.Strict (gets, modify)
import Convert.Diagnostic (Finding (..))
import Convert.Grammar.Constraint.Constraint08
    ( lSuperfixRoots
    , rSuperfixRoots
    , sSuperfixRoots
    )
import Convert.Grammar.Constraint.Constraint16
    ( suffixGroup16Da
    , suffixGroup16Sa
    )
import Convert.Grammar.Parser
    ( LeadRole (..)
    , SpellParser
    , WordTail (..)
    , claimFinal
    , liftP
    , noteFinding
    , resetFinalChain
    , scanLeading
    , scanTail
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable
    ( Position (..)
    , TibetanSyllable
    , caretMark
    , markS
    , subfixMarks
    )
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , tokenCanonical
    , tokenRaw
    , tokenSource
    )
import Data.Maybe (listToMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

-- | The word lead: a consonant-led or a vowel-led stack (the old pStack @t0@).
-- The lead stack is the only one that may open on a prefix letter (bsgribs ->
-- བ), because only a word's first stack is a prefix slot. A leading vowel
-- opens the word-tail count (3.4.4): whatever bare consonants follow it stand
-- in the word's suffix slots.
pConstraint21First :: SpellParser TibetanSyllable
pConstraint21First =
    MP.choice
        [ pStackBody True
        , vowelStart
        ]
    where
        vowelStart :: SpellParser TibetanSyllable
        vowelStart = do
            marks <- markS Vowel GP.pVowelAny
            resetWordTail
            -- A word that opens on a vowel opens a stack like any other.
            resetFinalChain
            pure marks

-- | Every way the word can continue: another consonant-led stack, a lone
-- vowel, a lone final mark (a caret among them), a subjoined letter the
-- previous stack could not keep, a forced join the previous stack closed too
-- soon to swallow (@u+e@ -> ཨེུ, @rH+e@ -> རཿེ), and a stack-breaking dot; a
-- dotted stack still spells as one word (g.yon -> གཡོན). A lone vowel starts
-- the word-tail count over (3.4.4), and a continuation that is not a
-- consonant-led stack ends the head letter the word opened on.
--
-- A lone final mark here is the same window the tail of a stack runs, and it
-- goes through the same 'oneFinalMark': which of the two claims a given token
-- depends on how the stack before it closed (a Wylie @a@ is a vowel the tail
-- can absorb, a Tibetan one is not, so a Tibetan @ཀཾཾ@ reaches this arm and
-- @kaMM@ does not), and that must not change what the spelling means.
pConstraint21Rest :: SpellParser TibetanSyllable
pConstraint21Rest =
    MP.choice
        [ noLead pDotBreak
        , noLead restJoin
        , vowelContinuation
        , noLead oneFinalMark
        , noLead (markS Subfix GP.pSubConsonant)
        , pStackBody False
        ]
    where
        -- A lone vowel that continues the word (dagsu): the word-tail count
        -- starts over at its last real vowel.
        vowelContinuation :: SpellParser TibetanSyllable
        vowelContinuation = do
            marks <- markS Vowel GP.pVowelAny
            resetWordTail
            pure marks

        -- A forced join in the word's rest that ends in a vowel (@u+e@)
        -- opens the tail the same way.
        restJoin :: SpellParser TibetanSyllable
        restJoin = do
            marks <- pForcedJoin
            when (startsWithVowel marks) resetWordTail
            pure marks

        -- A continuation that is not a consonant-led stack ends the head
        -- letter the word opened on: nothing after a dot, a join or a mark
        -- can be the stack a bare prefix or superfix was waiting for.
        noLead :: SpellParser a -> SpellParser a
        noLead p = p <* clearLead

-- | A dot that splits one stack into two (g.yon): only a dot followed by a
-- consonant reads this way, otherwise the word ends before it and the dot is
-- left for the sentence. Tibetan token streams never carry a dot.
pDotBreak :: SpellParser TibetanSyllable
pDotBreak =
    MP.try (liftP GP.pDot <* MP.lookAhead (MP.satisfy GP.isConsonantToken))
        *> resetFinalChain
        *> pure mempty

pStackBody :: Bool -> SpellParser TibetanSyllable
pStackBody atStart = do
    base <- liftP GP.pConsonant
    -- A new stack closes its own finals: the chain the previous one filled
    -- does not reach into it.
    resetFinalChain
    next0 <-
        MP.lookAhead (MP.skipMany (liftP GP.pCaret) *> MP.optional (liftP GP.pToken))
    let (pos, _) = markBase atStart base next0
        baseMark = Seq.singleton (pos, base)
    MP.choice
        [ completeStack base baseMark
        , bare base baseMark pos
        ]
    where
        -- The subjoining run under the base is over and the stack next meets
        -- a vowel or a @+@: spell the whole stack - the base, the subjoined
        -- letters, the caret, and the whole tail. Fails and rolls back to the
        -- bare base when neither follows.
        completeStack :: Token -> TibetanSyllable -> SpellParser TibetanSyllable
        completeStack base baseMark = MP.try $ do
            (subjoined, carets) <- liftP GP.pSubjoinRun
            -- The carets of the run belong to the chain before the tail reads
            -- it: only the first prints below the run (g^r^a), and a lone caret
            -- after the stack is a second one (g^ra^).
            noteRunCarets carets
            next <- MP.lookAhead (MP.optional (liftP GP.pToken))
            case next of
                Just t | GP.isVowelLike t || GP.isPlus t -> do
                    -- The stack the pending head letter opened now stands
                    -- with its vowel: a superfix is checked against its root
                    -- and combination tables, a prefix that reached a vowel
                    -- is exactly what its rule asks for; either way the
                    -- pending lead is over.
                    settleVowelHead base subjoined
                    tailMarks <- pConsumeTail False
                    pure
                        (baseMark <> subfixMarks subjoined <> caretMark (listToMaybe carets) <> tailMarks)
                _ -> MP.empty

        -- The bare head (3.4.3): the word's first stack closed bare, so the
        -- grammar itself has the head letter and its role in hand. Wave 3.4.4
        -- adds the windows that sit on every bare stack: the pending head
        -- letter this stack closes - a prefix that reached no vowel, or a
        -- superfix that gated nothing - and the suffix-count window of the
        -- word tail.
        bare :: Token -> TibetanSyllable -> Position -> SpellParser TibetanSyllable
        bare base baseMark pos = do
            when atStart (checkHead base pos)
            settleBareHead base pos
            advanceWordTail base
            pure baseMark

        -- The pending head letter, now that the stack it opened stands with
        -- a vowel: a superfix is checked against its tables, a prefix that
        -- reached a vowel is what its rule asks for; either way the pending
        -- lead is over.
        settleVowelHead :: Token -> [Token] -> SpellParser ()
        settleVowelHead base subjoined = do
            pending <- gets scanLeading
            case pending of
                Just (LeadSuperfix, lead) -> checkSuperfixCombination lead base subjoined
                Just (LeadPrefix, _) -> pure ()
                Nothing -> pure ()
            clearLead

        -- The pending head letter, now that the stack it opened closed bare:
        -- a prefix whose root reached no vowel is the rule of the vowel after
        -- the prefix (@bk@); a superfix that gates nothing closes silently
        -- (@rk@). This bare stack may itself open the next pending lead.
        settleBareHead :: Token -> Position -> SpellParser ()
        settleBareHead base pos = do
            pending <- gets scanLeading
            case pending of
                Just (LeadPrefix, lead) ->
                    noteFinding (NoVowelAfterPrefix (tokenRaw lead))
                Just (LeadSuperfix, _) -> pure ()
                Nothing -> pure ()
            setLead $
                case pos of
                    Superfix -> Just (LeadSuperfix, base)
                    Prefix -> Just (LeadPrefix, base)
                    _ -> Nothing

        -- The suffix-count window of the word tail (4.15/4.16): after the
        -- last real vowel of the word (the grammar resets the count at every
        -- vowel it places) the bare single consonants stand in the suffix
        -- slots. The first fills the first slot; the second only fits when
        -- the postfix pair of rule 4.16 stands - otherwise the slot is the
        -- window where the pair rule fires - and a third consonant can follow
        -- no 2nd suffix. A bare superfix counts like any bare stack (dagsg
        -- reads its second consonant there).
        advanceWordTail :: Token -> SpellParser ()
        advanceWordTail t = do
            wt <- gets scanTail
            case wt of
                TailVoid -> pure ()
                TailOpen Nothing Nothing -> setTail (TailOpen (Just t) Nothing)
                TailOpen (Just c1) Nothing ->
                    case secondSuffixOf c1 t of
                        Right () -> setTail (TailOpen (Just c1) (Just t))
                        Left finding -> noteFinding finding
                TailOpen (Just _) (Just _) ->
                    noteFinding (ConsonantAfter2ndSuffix (tokenRaw t))
                -- Unreachable: the count fills the first slot before the
                -- second. When it ever surfaces, the second slot is filled
                -- anyway, so a consonant may follow none of it.
                TailOpen Nothing (Just _) ->
                    noteFinding (ConsonantAfter2ndSuffix (tokenRaw t))

        -- Rule 4.16 as a predicate on the pair: @s@ follows @g ng b m@ and
        -- @d@ follows @n r l@ (the sets live in Constraint16, the book's own
        -- 4.16 data); a consonant that is no 2nd-suffix letter at all gets its
        -- own finding, and one of the two postfix letters over the wrong first
        -- suffix carries the pair that failed.
        secondSuffixOf :: Token -> Token -> Either Finding ()
        secondSuffixOf c1 c2 =
            case tokenCanonical c2 of
                TcConsonant Cs
                    | pairsWith suffixGroup16Sa c1 -> Right ()
                TcConsonant Cd
                    | pairsWith suffixGroup16Da c1 -> Right ()
                TcConsonant Cs ->
                    Left (BadSecondSuffix (tokenRaw c2) (Just (tokenRaw c1)))
                TcConsonant Cd ->
                    Left (BadSecondSuffix (tokenRaw c2) (Just (tokenRaw c1)))
                _ -> Left (BadSecondSuffix (tokenRaw c2) Nothing)
            where
                pairsWith :: [Consonant] -> Token -> Bool
                pairsWith allowed first = case tokenCanonical first of
                    TcConsonant c -> c `elem` allowed
                    _ -> False

        setTail :: WordTail -> SpellParser ()
        setTail wt = modify $ \s -> s{scanTail = wt}

        setLead :: Maybe (LeadRole, Token) -> SpellParser ()
        setLead lead' = modify $ \s -> s{scanLeading = lead'}

-- | The superfix legality window: the pending superfix letter and the root
-- the stack gated, with the letters it subjoined. The tables (4.8, 5.1) are
-- the grammar's own data ('superfixRoots', 'superfixCombos'); outside them
-- the combination never stands, and the window names it.
checkSuperfixCombination :: Token -> Token -> [Token] -> SpellParser ()
checkSuperfixCombination lead base subjoined =
    case superfixCombination
        (tokenCanonical lead)
        (consonantOf base)
        (map consonantOf subjoined) of
        Just False ->
            noteFinding
                (BadSuperfixCombination (tokenRaw lead) (tokenRaw base) (map tokenRaw subjoined))
        _ -> pure ()

-- | The roots each superfix letter may gate, read from 'Constraint08' (rule
-- 4.8, its own data): the same sets the book's structure 2 accepts, stated
-- once there as the data this window reads.
superfixRoots :: Consonant -> [Consonant]
superfixRoots Cr = rSuperfixRoots
superfixRoots Cl = lSuperfixRoots
superfixRoots Cs = sSuperfixRoots
superfixRoots _ = []

-- | The (root, subfix) rows a superfix letter takes below those roots
-- (rules 4.8, 5.1): one letter only, so a run of two never matches a row;
-- @l@ takes none.
superfixCombos :: Consonant -> [(Consonant, Consonant)]
superfixCombos Cr =
    [ (Ck, Cy)
    , (Cg, Cy)
    , (Cm, Cy)
    , (Cb, Cw)
    , (Cts, Cw)
    , (Cg, Cw)
    ]
superfixCombos Cl = []
superfixCombos Cs =
    [ (Ck, Cy)
    , (Cg, Cy)
    , (Cp, Cy)
    , (Cb, Cy)
    , (Cm, Cy)
    , (Ck, Cr)
    , (Cg, Cr)
    , (Cp, Cr)
    , (Cb, Cr)
    , (Cm, Cr)
    , (Cn, Cr)
    ]
superfixCombos _ = []

-- | Whether the pending superfix letter gates the given root with the given
-- subjoined letters: the root must be in its 4.8 set and, when the stack
-- subjoined letters, the (root, subfix) pair must be in its table. Only the
-- three superfix letters decide the combination at all; anyone else - a
-- defensive branch, the pending lead never carries another role - is not a
-- combination question.
superfixCombination ::
    TokenCanonical -> Maybe Consonant -> [Maybe Consonant] -> Maybe Bool
superfixCombination (TcConsonant sf) (Just root) subs
    | sf `elem` [Cr, Cl, Cs] =
        Just $
            root `elem` superfixRoots sf
                && case subs of
                    [] -> True
                    [Just sub] -> (root, sub) `elem` superfixCombos sf
                    _ -> False
superfixCombination _ _ _ = Nothing

-- | The consonant a token names, however it was spelled: a bare letter in
-- Wylie, an already-joined sign in Tibetan. The subconsonants of the subfix
-- grid map to their letters for the superfix tables; any other sign names no
-- letter of the tables.
consonantOf :: Token -> Maybe Consonant
consonantOf tok =
    case tokenCanonical tok of
        TcConsonant c -> Just c
        TcSubConsonant sc -> subConsonantLetter sc
        _ -> Nothing

subConsonantLetter :: SubConsonant -> Maybe Consonant
subConsonantLetter SCw = Just Cw
subConsonantLetter SCy = Just Cy
subConsonantLetter SCr = Just Cr
subConsonantLetter SCl = Just Cl
subConsonantLetter _ = Nothing

-- | The pending head letter is over: nothing may survive a completed stack
-- or a continuation that is not a consonant-led stack.
clearLead :: SpellParser ()
clearLead = modify $ \s -> s{scanLeading = Nothing}

-- | The word-tail count starts over at a vowel: after its last real vowel a
-- word leaves the suffix slots open for the bare consonants that follow.
resetWordTail :: SpellParser ()
resetWordTail = modify $ \s -> s{scanTail = TailOpen Nothing Nothing}

-- | Whether a forced join ended in a vowel (@+e@).
startsWithVowel :: TibetanSyllable -> Bool
startsWithVowel w = case Seq.lookup 0 w of
    Just (Vowel, _) -> True
    _ -> False

-- | The rest of a complete stack - consecutive vowels (@a@ among them, the
-- letter that never prints), the caret and then finals, and forced subjoins
-- (@+X@, each possibly pulling its own subjoining run, g+mra). After a real
-- vowel the tail is over: a bare @a@ that follows (goang གོཨང) is the root ཨ
-- of a fresh stack, not the implicit vowel again, so the a-chen branch only
-- fires before the first vowel.
--
-- The chain of finals belongs to the stack, not to the tail parser: it is the
-- state in 'ScanState', so a second stack starts a new one and both windows
-- that claim a final mark read the same spelling the same way. Every vowel
-- placed here, a real one, the implicit @a@, or a vowel forced in with @+@,
-- starts the word's suffix count over.
pConsumeTail :: Bool -> SpellParser TibetanSyllable
pConsumeTail vowelSeen =
    MP.choice
        [ pTailVowel
        , pImplicitABranch
        , pTailFinal
        , pForcedJoinBranch
        , pure mempty
        ]
    where
        -- A real vowel, then the rest of the tail after it.
        pTailVowel :: SpellParser TibetanSyllable
        pTailVowel = MP.try $ do
            marks <- markS Vowel GP.pVowelAny
            resetWordTail
            rest <- pConsumeTail True
            pure (marks <> rest)

        -- The bare @a@ only fits before the first real vowel of the stack.
        pImplicitABranch :: SpellParser TibetanSyllable
        pImplicitABranch
            | vowelSeen = MP.empty
            | otherwise = MP.try $ do
                marks <- markS ImplicitVowel GP.pImplicitA
                resetWordTail
                rest <- pConsumeTail False
                pure (marks <> rest)

        -- One final mark, then the rest of the tail: a final does not place a
        -- vowel, so the flag the stack reached is the one the tail keeps.
        pTailFinal :: SpellParser TibetanSyllable
        pTailFinal = MP.try $ do
            marks <- oneFinalMark
            rest <- pConsumeTail vowelSeen
            pure (marks <> rest)

        -- A forced join may itself be a vowel (@+e@): the tail after it then
        -- continues as after any real vowel. A forced join that drags a
        -- consonant below a stack whose vowel is already placed is the
        -- @+@-after-vowel rule of 4.21.
        pForcedJoinBranch :: SpellParser TibetanSyllable
        pForcedJoinBranch = MP.try $ do
            marks <- pForcedJoin
            when (vowelSeen && not (startsWithVowel marks)) $
                noteFinding (JoinAfterVowel (joinHeadRaw marks))
            when (startsWithVowel marks) resetWordTail
            rest <- pConsumeTail (vowelSeen || startsWithVowel marks)
            pure (marks <> rest)

        -- The letter a forced join dragged below the stack, for the wording
        -- of the @+@-after-vowel window (@ku+k@ blames @k@).
        joinHeadRaw :: TibetanSyllable -> Text
        joinHeadRaw w = case Seq.lookup 0 w of
            Just (Subfix, tok) -> T.filter (/= '+') (tokenRaw tok)
            _ -> T.empty

-- | The carets a subjoining run swallowed are spent, and the chain has to know
-- it: a caret fills no slot but it is a caret the stack carries, so a lone
-- caret closing the syllable after such a stack is a second one and the tail
-- window has to say so. Both windows that take a caret out of a run - the
-- plain stack and the forced join - come through here, so the chain is written
-- in one place instead of one place per window.
--
-- The surplus of a run is counted one caret at a time, the same rule the tail
-- applies. Only the first caret of a run prints below it; the rest are the
-- window's own finding, which is what @g^r^a@ has always done.
noteRunCarets :: [Token] -> SpellParser ()
noteRunCarets = mapM_ noteRunCaret
    where
        noteRunCaret :: Token -> SpellParser ()
        noteRunCaret tok = do
            fit <- claimFinal tok
            case fit of
                GP.FinalRepeatedCaret -> noteFinding SecondCaret
                _ -> pure ()

-- | One final mark, taken against the chain this stack's earlier finals built
-- and marked 'Eaten' when it does not fit: the mark stays in the parse and in
-- the run, the finding names it, and it prints nothing - which is what the
-- second caret of @g^r^a@ already does here, and what the reference does with
-- the mark it drops (@oMM@ -> ཨོཾ).
--
-- Both windows that claim a final mark come through this one. Which window
-- claims a spelling is a matter of how the stack before it closed - a Wylie
-- @a@ is a vowel the tail can absorb, a Tibetan one is not - and that must not
-- change what the spelling means: @kaMM@ and @aMM@ are one spelling, and the
-- reference reads both the same way.
oneFinalMark :: SpellParser TibetanSyllable
oneFinalMark = MP.try $ do
    tok <- liftP GP.pFinal
    fit <- claimFinal tok
    case fit of
        GP.FinalFits -> pure (Seq.singleton (Final, tok))
        GP.FinalOutOfChain -> do
            noteFinding (DuplicateFinalMark (tokenRaw tok))
            pure (Seq.singleton (Eaten, tok))
        GP.FinalRepeatedCaret -> do
            noteFinding SecondCaret
            pure (Seq.singleton (Eaten, tok))

-- | The forced join sign @+@ (Wylie syntax only), with two readings: the
-- letter after it is dragged below the stack as a subfix together with its
-- own subjoining run ('pSubjoinBelow', g+mra -> གྨྲ), or - when the stack
-- already had its vowel - the sign is followed by that stack's vowel
-- (bru+e -> བྲེུ). The a-chen (s+a -> སྸ) is a consonant like any other, so
-- it falls into the join-below reading. The same parser reads a sign the
-- stack already closed, because the references read it the same way the open
-- stack does: the vowel or consonant after it still belongs to the same word
-- (u+e -> ཨེུ, rH+e -> རཿེ). 'pConstraint21Rest' reaches for it there.
pForcedJoin :: SpellParser TibetanSyllable
pForcedJoin = MP.try $ do
    void (liftP GP.pPlus)
    MP.choice
        [ MP.try pSubjoinBelow
        , markS Vowel GP.pVowelAny
        ]
    where
        -- \| The letter a forced join drags below the stack - a consonant or the
        -- a-chen, already subjoined or not - with the subjoining run under it. The
        -- forced letter leads and the run's letters trail in reverse, exactly where
        -- they print (g+mra -> གྨྲ); a caret that survived the run lands between them
        -- (f+ra -> ཕ༹ྲ).
        pSubjoinBelow :: SpellParser TibetanSyllable
        pSubjoinBelow = do
            next <- MP.choice [liftP GP.pConsonant, liftP GP.pSubConsonant]
            (subjoined, carets) <- liftP GP.pSubjoinRun
            noteRunCarets carets
            pure $
                Seq.singleton (Subfix, next)
                    <> caretMark (listToMaybe carets)
                    <> Seq.fromList [(Subfix, s) | s <- reverse subjoined]

-- | Where the base stands: a word-initial prefix letter over a consonant is a
-- Prefix (bsgribs -> བ), a superfix letter over a non-subfix consonant is a
-- Superfix (rka -> རྐ), anything else is the Root. The renderer prints the
-- letter either way; the positions matter to the legality rules of wave 3.4.
markBase :: Bool -> Token -> Maybe Token -> (Position, Token)
markBase atStart base next =
    case next of
        Just n
            -- The letter directly below the base belongs to its own stack
            -- (bra -> བྲ), so a subjoining letter is never a prefix.
            | atStart
                && GP.isPrefixLetter base
                && GP.isConsonantToken n
                && not (GP.isSubjoinCandidate n)
                && not (GP.isImplicitA n) ->
                (Prefix, base)
            | GP.isSuperfixLetter base
                && not (GP.isSubjoinCandidate n)
                && not (GP.isImplicitA n) ->
                (Superfix, base)
        _ -> (Root, base)

-- | The head check (3.4.3), fired when the word's first stack closed bare:
-- the grammar placed the head letter with a role that is not a superfix stack
-- (@rka@/@sgra@ hold a stack and never warn) and the head is a written Wylie
-- consonant (the a-chen is never a prefix). Unicode runs never sit in the
-- prefix position: the prefix warnings are a W->U notion. The check decides
-- by the pair predicate 'prefixAllows', like any other check of the grammar:
-- no tables are probed over the raw run after the parse.
checkHead :: Token -> Position -> SpellParser ()
checkHead base pos =
    when
        (pos /= Superfix && tokenSource base == TsWylie)
        ( case tokenCanonical base of
            TcConsonant c
                | c == Ca || T.null (tokenRaw base) -> pure ()
                | GP.isPrefixLetter base -> do
                    -- The letter right behind the bare head; a stack dot the
                    -- run opens with is skipped, so g.yag names @y@ and
                    -- g....yag names the second dot, exactly as the spelling
                    -- reads back.
                    next <-
                        MP.lookAhead
                            (liftP (MP.optional GP.pDot) *> liftP (MP.optional GP.pToken))
                    checkPrefixLead base next
                | otherwise ->
                    noteFinding (HeadNotAPrefix (tokenRaw base))
            _ -> pure ()
        )

-- | Whether the next letter may follow the prefix head: a letter the prefix
-- leads stays silent; a stack dot or a letter outside its section 4.2 set is
-- exactly what the warning names; nothing behind the head means nothing to
-- lead.
checkPrefixLead :: Token -> Maybe Token -> SpellParser ()
checkPrefixLead base next =
    case next of
        Nothing -> pure ()
        Just n
            | prefixAllows base n -> pure ()
            | otherwise ->
                noteFinding
                    (HeadPrefixCannotLead (tokenRaw base) (T.filter (/= '+') (tokenRaw n)))

-- | Section 4.2 as a predicate on the head and the next token: the letters a
-- prefix may lead are the roots of its own set, plus the @r@/@l@ superfixes
-- the @b@-prefix opens. The pair is decided token by token, in the head
-- window of this constraint, exactly as the other windows decide theirs.
prefixAllows :: Token -> Token -> Bool
prefixAllows headTok nextTok =
    case (tokenCanonical headTok, tokenCanonical nextTok) of
        (TcConsonant Cg, TcConsonant c) -> c `elem` [Cc, Cny, Ct, Cd, Cn, Cts, Czh, Cz, Cy, Csh, Cs]
        (TcConsonant Cd, TcConsonant c) -> c `elem` [Ck, Cg, Cng, Cp, Cb, Cm]
        (TcConsonant Cb, TcConsonant c) -> c `elem` [Ck, Cg, Cc, Ct, Cd, Cts, Czh, Cz, Csh, Cs, Cr, Cl]
        (TcConsonant Cm, TcConsonant c) -> c `elem` [Ckh, Cg, Cng, Cch, Cj, Cny, Cth, Cd, Cn, Ctsh, Cdz]
        (TcConsonant C', TcConsonant c) -> c `elem` [Ckh, Cg, Cch, Cj, Cth, Cd, Cph, Cb, Ctsh, Cdz]
        _ -> False
