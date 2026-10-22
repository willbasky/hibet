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
unless it is a superfix letter gating a root ('placeBase' says so; @rka@ and
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
    , closeWord
    ) where

import Control.Monad (void, when)
import Control.Monad.Trans.State.Strict (gets, modify)
import Convert.Diagnostic (Finding (..))
import Convert.Grammar.Constraint.Constraint16
    ( suffixGroup16Da
    , suffixGroup16Sa
    )
import Convert.Grammar.Parser
    ( LeadRole (..)
    , SpellParser
    , WordTail (..)
    , caretMark
    , claimFinal
    , liftP
    , markImplicitVowel
    , markSubfix
    , markVowel
    , noteFinding
    , peekStacks
    , placeFinal
    , placePrefix
    , placeRoot
    , placeSubfix
    , placeSuperfix
    , resetFinalChain
    , scanLeading
    , scanStacks
    , scanTail
    , subfixMarks
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Superfix (superfixCombos, superfixRoots)
import Convert.Grammar.Syllable
    ( Role (..)
    , Stack (..)
    , Subfixes (NoSubfix)
    , TibetanSyllable
    , VowelSlot (..)
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
import Data.Foldable (toList)
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
            marks <- markVowel GP.pVowelAny
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
        , noLead (markSubfix GP.pSubConsonant)
        , pStackBody False
        ]
    where
        -- A lone vowel that continues the word (dagsu): the word-tail count
        -- starts over at its last real vowel. Only while the word's last stack
        -- still waits for its vowel does the sign fill it; once that stack
        -- carries one - the a-chen itself, the implicit @a@ or a sign - the
        -- sign is a fresh a-chen stack of its own (ae -> ཨ + ཨེ, kae -> ཀ +
        -- ཨེ), and 'pSyllable' reads it as a fresh item, as a dot already
        -- forces for ba.e.
        vowelContinuation :: SpellParser TibetanSyllable
        vowelContinuation = do
            taken <- stackVowelTaken
            if taken
                then MP.empty
                else do
                    marks <- markVowel GP.pVowelAny
                    resetWordTail
                    pure marks

        -- A forced join in the word's rest that ends in a vowel (@u+e@)
        -- opens the tail the same way - and is a vowel of the stack it joins,
        -- so the fresh stack behind it starts a chen of its own.
        restJoin :: SpellParser TibetanSyllable
        restJoin = do
            marks <- pForcedJoin
            when (startsWithVowel marks) $ resetWordTail
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

-- | The word is over: the sign that closes its tail is a suffix (REFACTOR-AST
-- §8 A5). The closing sign is the last stack, when it is a bare consonant
-- behind the word's root - no written vowel (a second root carries one), no
-- superfix (that stack is a root), and not the root stack itself. Everything
-- before the closing sign keeps its role: a second root with its own vowel
-- (@khye'us@, @pa'e@) and the a-chung that carries the implicit @a@ of the
-- spelling (@pa'am@) are no suffixes. The book is silent about tails outside
-- 4.15/4.16/4.20, so this is our rule, named here and not guessed by a
-- renderer.
closeWord :: SpellParser ()
closeWord = modify $ \s -> s{scanStacks = closeTail (scanStacks s)}

closeTail :: Seq.Seq Stack -> Seq.Seq Stack
closeTail stacks = case Seq.viewr stacks of
    rest Seq.:> top
        | closesTail rest top -> rest Seq.|> top{stRole = RoleSuffix}
    _ -> stacks
    where
        closesTail rest top =
            stRole top == RoleRoot
                && stSuperfix top == Nothing
                && stSubfixes top == NoSubfix
                && stVowel top == Nothing
                && maybe False isSuffixLetter (stRoot top)
                && any isRoot (toList rest)

        isRoot st = stRole st == RoleRoot && stRoot st /= Nothing

        -- The ten closing letters of classical orthography (§8 A5). A letter
        -- outside them is no suffix: ཨཟ is a second root with implicit @a@.
        isSuffixLetter tok = case tokenCanonical tok of
            TcConsonant c -> c `elem` GP.suffixConsonants
            _ -> False

pStackBody :: Bool -> SpellParser TibetanSyllable
pStackBody atStart = do
    base <- liftP GP.pConsonant
    -- A new stack closes its own finals: the chain the previous one filled
    -- does not reach into it.
    resetFinalChain
    next0 <-
        MP.lookAhead (MP.skipMany (liftP GP.pCaret) *> MP.optional (liftP GP.pToken))
    -- The letters the root behind the base would subjoin. Only a superfix
    -- letter asks: rule 4.10 puts la below no superfix at all, so whether the
    -- gate shuts is known here, where the base is placed - and the bare stack
    -- that names the combination quotes this very run.
    below0 <-
        if GP.isSuperfixLetter base
            then MP.lookAhead $ do
                _ <- MP.skipMany (liftP GP.pCaret)
                _ <- MP.optional (liftP GP.pToken)
                fst <$> liftP GP.pSubjoinRun
            else pure []
    -- The base goes into the stack before the choice below: whichever arm wins
    -- keeps it, and a failed arm is rolled back to this point, not past the
    -- letter that opened the stack. Its slot is decided by 'placeBase'; how it
    -- stands is then read from the stack it opened.
    baseMark <- placeBase atStart base next0 below0
    standing <- gets (maybe (RoleRoot, False) standingOf . lastStack . scanStacks)
    MP.choice
        [ completeStack base baseMark
        , bare base baseMark standing next0 below0
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
                    -- The letters below the base are written before the vowel
                    -- that ends the stack, so they are placed before the tail
                    -- reads it; the tail then finds the stack it joins.
                    subfixMarks' <- subfixMarks subjoined
                    caret <- caretMark (listToMaybe carets)
                    tailMarks <- pConsumeTail False (GP.isImplicitAWylie base)
                    pure (baseMark <> subfixMarks' <> caret <> tailMarks)
                _ -> MP.empty

        -- The bare head (3.4.3): the word's first stack closed bare, so the
        -- grammar itself has the head letter and its role in hand. Wave 3.4.4
        -- adds the windows that sit on every bare stack: the pending head
        -- letter this stack closes - a prefix that reached no vowel, or a
        -- superfix that gated nothing - and the suffix-count window of the
        -- word tail.
        bare ::
            Token
            -> TibetanSyllable
            -> (Role, Bool)
            -> Maybe Token
            -> [Token]
            -> SpellParser TibetanSyllable
        bare base baseMark (role, over) next below = do
            named <- noteShutGate base next below
            when (atStart && not named) (checkHead base over)
            settleBareHead base role over
            advanceWordTail base
            pure baseMark

        -- The gate rule 4.10 keeps shut: la below the root keeps the base an
        -- ordinary root ('placeBase'), so no pending lead reaches
        -- 'settleVowelHead'. This bare stack therefore calls the same table
        -- window on the same run, and the finding is the one that window
        -- would name (grglam). The same window now names a superfix letter
        -- whose root lies outside its 4.8 set (rpa, rsa): the gate of rule
        -- 4.8 shut it here, and the word head is still the place the writer
        -- reached for a superfix.
        noteShutGate :: Token -> Maybe Token -> [Token] -> SpellParser Bool
        noteShutGate base next below = case next of
            Just n
                | Just run <- superfixRun base n below
                , any isLa run || not (gatedSuperfix base n)
                , atStart || any isLa run ->
                    checkSuperfixCombination base n run *> pure True
            _ -> pure False

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
        settleBareHead :: Token -> Role -> Bool -> SpellParser ()
        settleBareHead base role over = do
            pending <- gets scanLeading
            case pending of
                Just (LeadPrefix, lead) ->
                    noteFinding (NoVowelAfterPrefix (tokenRaw lead))
                Just (LeadSuperfix, _) -> pure ()
                Nothing -> pure ()
            setLead $
                case role of
                    _ | over -> Just (LeadSuperfix, base)
                    RolePrefix -> Just (LeadPrefix, base)
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

-- | Whether the word's last open stack already carries its vowel. A written
-- sign closes the slot, the bare implicit @a@ closes it, and a Wylie a-chen
-- base is the vowel itself - unless a forced join has subjoined a letter below
-- it (@a+yo@ -> ཨྱོ), which turns the base into a root still waiting for one.
-- A long @a@ token that the bare short vowel of its own mnemonic follows
-- (@I@ -> @A@ + @i@, the second token written by nothing, see
-- 'GP.pLongVowelHalf') leaves the slot open: the completion joins the same
-- stack.
stackVowelTaken :: SpellParser Bool
stackVowelTaken = do
    top <- gets (lastStack . scanStacks)
    case top of
        Nothing -> pure False
        Just st -> case stVowel st of
            Just VowelImplicit -> pure True
            Just (VowelWritten tok rest)
                | null rest && GP.isLongAVowelToken tok -> do
                    half <-
                        liftP
                            (MP.option False (MP.lookAhead (True <$ GP.pLongVowelHalf)))
                    pure (not half)
                | otherwise -> pure True
            Nothing -> pure (isAChenRoot st)
    where
        isAChenRoot st = case stRoot st of
            Just tok -> GP.isImplicitAWylie tok && stSubfixes st == NoSubfix
            Nothing -> False

-- | Whether a forced join ended in a vowel (@+e@).
startsWithVowel :: TibetanSyllable -> Bool
startsWithVowel = maybe False GP.isEatableVowel . Seq.lookup 0

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
-- | The tail of a stack after its base stands: a vowel, the final marks, the
-- forced joins. @baseIsA@ says the base of this very stack is the Wylie
-- a-chen - the letter that already is the vowel an implicit @a@ would spell
-- there - so the tail may not place another one (@aa@ is two stacks, as the
-- reference reads it). A written Tibetan a-chen is no such base: it is a
-- letter like any consonant and takes its own vowel (ཨེ is one stack).
pConsumeTail :: Bool -> Bool -> SpellParser TibetanSyllable
pConsumeTail vowelSeen baseIsA =
    MP.choice
        [ pTailVowel
        , pImplicitABranch
        , pTailFinal
        , pForcedJoinBranch
        , pForcedFinalBranch
        , pure mempty
        ]
    where
        -- A real vowel, then the rest of the tail after it. A stack carries a
        -- single vowel, so once one stands - the sign read here, the implicit
        -- @a@ of 'pImplicitABranch', or the a-chen base itself - no other
        -- joins it and the next sign opens a fresh stack (kie -> ཀི + ཨེ,
        -- kae -> ཀ + ཨེ).
        pTailVowel :: SpellParser TibetanSyllable
        pTailVowel
            | vowelSeen || baseIsA = MP.empty
            | otherwise = MP.try $ do
                marks <- markVowel GP.pVowelAny
                resetWordTail
                rest <- pConsumeTail True baseIsA
                pure (marks <> rest)

        -- The bare Wylie @a@: the letter that stands for the implicit vowel.
        -- It fits only before the stack's first vowel, never after one and
        -- never as the vowel of a Wylie a-chen base, which is what makes @aa@
        -- and @ae@ two stacks. It fills the slot it takes, so a sign behind
        -- it opens a fresh stack (@kae@ -> ཀ + ཨེ). A written Tibetan a-chen
        -- is no such letter and never reaches here.
        pImplicitABranch :: SpellParser TibetanSyllable
        pImplicitABranch
            | vowelSeen || baseIsA = MP.empty
            | otherwise = MP.try $ do
                marks <- markImplicitVowel GP.pImplicitAWylie
                resetWordTail
                rest <- pConsumeTail True baseIsA
                pure (marks <> rest)

        -- One final mark, then the rest of the tail: a final places no vowel,
        -- so the vowel state the tail reached is the one it keeps.
        pTailFinal :: SpellParser TibetanSyllable
        pTailFinal = MP.try $ do
            marks <- oneFinalMark
            rest <- pConsumeTail vowelSeen baseIsA
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
            -- The letter a forced join dragged below a stack that already has
            -- its vowel spells with the stack's implicit /a/: the @a@ after
            -- @+r@ in bo+ra is that vowel (བོྲ), not the root ཨ of a fresh
            -- stack - the a-chen branch below only fires before the first
            -- vowel, so the tail cannot take the letter there.
            inheritVowel <- case Seq.lookup 0 marks of
                Just t
                    | not (GP.isEatableVowel t) ->
                        MP.option mempty (markImplicitVowel GP.pImplicitA)
                _ -> pure mempty
            -- A vowel of the join (a @+e@, or the implicit @a@ it pulled in)
            -- fills the stack; a join that only dragged a letter below a fresh
            -- a-chen base turns that base into a root still waiting for one
            -- (a+yo -> ཨྱོ) - 'stackVowelTaken' reads both from the stack.
            rest <- pConsumeTail (vowelSeen || startsWithVowel marks) baseIsA
            pure (marks <> inheritVowel <> rest)

        -- The forced join sign with a final behind it: the reference lets the
        -- stack's own subscript loop take the sign and its finals section
        -- take the letter after it (@g+X@ -> ག༷), so the two belong to this
        -- stack - and the final closes it, the word loop reads on from there.
        pForcedFinalBranch :: SpellParser TibetanSyllable
        pForcedFinalBranch = MP.try $ do
            void (liftP GP.pPlus)
            oneFinalMark

        -- The letter a forced join dragged below the stack, for the wording
        -- of the @+@-after-vowel window (@ku+k@ blames @k@).
        joinHeadRaw :: TibetanSyllable -> Text
        joinHeadRaw w = case Seq.lookup 0 w of
            Just tok
                | not (GP.isEatableVowel tok) ->
                    T.filter (/= '+') (tokenRaw tok)
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

-- | One final mark, taken against the chain this stack's earlier finals built.
-- A mark that does not fit is dropped: the finding names it, and it prints
-- nothing - which is what the second caret of @g^r^a@ already does here, and
-- what the reference does with the mark it drops (@oMM@ -> ཨོཾ).
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
        GP.FinalFits -> placeFinal tok
        GP.FinalOutOfChain -> do
            noteFinding (DuplicateFinalMark (tokenRaw tok))
            pure mempty
        GP.FinalRepeatedCaret -> do
            noteFinding SecondCaret
            pure mempty

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
        , markVowel GP.pVowelAny
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
            first <- placeSubfix next
            caret <- caretMark (listToMaybe carets)
            deeper <- subfixMarks (reverse subjoined)
            pure (first <> caret <> deeper)

-- | Where the base stands: a word-initial prefix letter over a consonant is a
-- prefix (bsgribs -> བ), a superfix letter over a non-subfix consonant is a
-- superfix (rka -> རྐ), anything else is a root. The slot is the field of the
-- stack the base opens, and the legality rules of wave 3.4 read it from there.
--
-- One superfix letter gates nothing whatever: rule 4.10 keeps la below no
-- superfix (its 15 triples take wa, ya and ra only), so a root that would
-- subjoin it shuts the gate before it opens and the base is a root like any
-- other (grglam -> གརགླམ, as the reference reads it).
placeBase ::
    Bool -> Token -> Maybe Token -> [Token] -> SpellParser TibetanSyllable
placeBase atStart base next below =
    case next of
        Just n
            -- The letter directly below the base belongs to its own stack
            -- (bra -> བྲ), so a subjoining letter is never a prefix.
            | atStart
                && GP.isPrefixLetter base
                && GP.isConsonantToken n
                && not (GP.isSubjoinCandidate n)
                && not (GP.isImplicitA n)
                && prefixAllows base n ->
                placePrefix base
            | Just run <- superfixRun base n below
            , not (any isLa run)
            , gatedSuperfix base n ->
                placeSuperfix base
        _ -> placeRoot base

-- | How the stack a base just opened stands: its role, and whether the base sits
-- above it as a superfix.
standingOf :: Stack -> (Role, Bool)
standingOf st =
    (stRole st, case stSuperfix st of Just _ -> True; Nothing -> False)

-- | The last stack the run has built, if any.
lastStack :: Seq.Seq Stack -> Maybe Stack
lastStack stacks = case Seq.viewr stacks of
    Seq.EmptyR -> Nothing
    _ Seq.:> st -> Just st

-- | The gate of rule 4.8 from the base's own side: the letters the root
-- behind a superfix letter would subjoin, but only while that gate stands -
-- a superfix letter over a consonant of its own stacks, with nothing a subfix
-- or a vowel can close it on (rka, sgrwa). Both the placement and the window
-- that names a failed combination read this one predicate.
superfixRun :: Token -> Token -> [Token] -> Maybe [Token]
superfixRun base n below
    | GP.isSuperfixLetter base
    , not (GP.isSubjoinCandidate n)
    , not (GP.isImplicitA n) =
        Just below
    | otherwise = Nothing

-- | The root a superfix letter may gate, from rule 4.8's own sets: the letter
-- right behind the superfix has to be one of its roots. The reference gates
-- the same way (jsewts @isSuperscript t && superscript t t2@). Without it the
-- letter is no superfix at all and reads as a plain root (rsa -> ras); an
-- illegal subfix combination below a legal root still stacks and only the
-- window warns (sgrwa).
gatedSuperfix :: Token -> Token -> Bool
gatedSuperfix base n =
    case (tokenCanonical base, consonantOf n) of
        (TcConsonant sf, Just root) -> root `elem` superfixRoots sf
        _ -> False

-- | The letter la in either script - the Wylie letter and the Tibetan
-- subjoined ལ - the subfix rule 4.10 lets stand below no superfix at all.
isLa :: Token -> Bool
isLa t = case tokenCanonical t of
    TcConsonant Cl -> True
    TcSubConsonant SCl -> True
    _ -> False

-- | The head check (3.4.3), fired when the word's first stack closed bare:
-- the grammar placed the head letter with a role that is not a superfix stack
-- (@rka@/@sgra@ hold a stack and never warn) and the head is a written Wylie
-- consonant (the a-chen is never a prefix). Unicode runs never sit in the
-- prefix position: the prefix warnings are a W->U notion. The check decides
-- by the pair predicate 'prefixAllows', like any other check of the grammar:
-- no tables are probed over the raw run after the parse.
checkHead :: Token -> Bool -> SpellParser ()
checkHead base over =
    when
        (not over && tokenSource base == TsWylie)
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
