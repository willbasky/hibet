-- | Token-level sentence dispatcher.
--
-- Parses a token stream into a list of 'SpellItem' preserving every token:
-- syllables are recognized via the Convert.Grammar.Structure structures and
-- become the nested 'Convert.Grammar.Syllable.Syllable' of REFACTOR-AST,
-- Tibetan digits become 'Number', punctuation and whitespace tokens are kept
-- as 'Punct', and anything unrecognized is kept as 'Other' (nothing is
-- dropped).
--
-- A run is one stretch of tokens up to (not including) its boundary: the tsheg
-- or space that closes it is a 'Punct' item of its own (REFACTOR-AST Q4). The
-- bridge from the grammar's marked runs to the stacks is 'stacksOf' until the
-- grammar builds the stacks itself (step 4).
module Convert.Sentence
    ( SpellItem (..)
    , Syllable (..)
    , pSentence
    ) where

import Control.Monad (void, when)
import Convert.Diagnostic
    ( Diagnostics
    , Finding (FinalWithoutLetter, SignWithoutLetter, UnplacedDot)
    , findingsDiagnostics
    )
import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , Spelling (..)
    , isDot
    , isPunctuationLike
    , liftP
    , noteFinding
    , pNumber
    , pPunctuation
    , pToken
    , pUnclaimedTail
    , runSpell
    , takeFindings
    )
import Convert.Grammar.Structure
    ( pStructure1
    , pStructure10
    , pStructure11
    , pStructure12
    , pStructure13
    , pStructure14
    , pStructure15
    , pStructure16
    , pStructure17
    , pStructure18
    , pStructure19
    , pStructure2
    , pStructure20
    , pStructure21
    , pStructure22
    , pStructure23
    , pStructure24
    , pStructure25
    , pStructure26
    , pStructure27
    , pStructure28
    , pStructure29
    , pStructure3
    , pStructure30
    , pStructure31
    , pStructure32
    , pStructure33
    , pStructure34
    , pStructure35
    , pStructure36
    , pStructure37
    , pStructure38
    , pStructure4
    , pStructure5
    , pStructure6
    , pStructure7
    , pStructure8
    , pStructure9
    )
import Convert.Grammar.Syllable
    ( Position (..)
    , Role (..)
    , Stack (..)
    , Syllable (..)
    , TibetanSyllable
    , VowelSlot (..)
    , flatten
    , stacksOf
    )
import Convert.Token
    ( Consonant (Ca)
    , Span (..)
    , Token
    , TokenCanonical (..)
    , TokenSource (..)
    , isToplessMark
    , offsetEnd
    , offsetStart
    , tokenCanonical
    , tokenRaw
    , tokenSource
    , tokenSpan
    )
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

-- | One piece of a parsed sentence. A recognized run is a 'SyllableItem'; a
-- run that opens like a syllable but that no structure claims, and the signs a
-- claimed run could not place, are an 'InvalidSyllableItem' (kept as the tokens
-- they were written as, with the warnings that came with them) - such runs
-- stay visible and diagnosable instead of silently falling into 'Other'.
data SpellItem
    = SyllableItem Syllable
    | InvalidSyllableItem [Token] Diagnostics
    | Number [Token]
    | Punct [Token]
    | Other [Token]
    deriving (Show, Eq)

-- | The sentence parse as a plain token parse: the stateful grammar
-- ('pSentenceS') runs with a fresh state and the state is discarded - the
-- warnings the constraint windows produced are already spelled into each
-- run's 'sylDiags' - so the entry point stays as the pure 'Parser' the
-- converter and the tests drive.
pSentence :: Spelling -> Parser [SpellItem]
pSentence spelling = runSpell (pSentenceS spelling)

-- | The stateful sentence parse: 'StateT ScanState Parser' threads the
-- grammar state across the runs, and each 'pSyllable' takes the findings of
-- the run it owns.
pSentenceS :: Spelling -> SpellParser [SpellItem]
pSentenceS spelling = concat <$> MP.many (pItem spelling) <* MP.eof

pItem :: Spelling -> SpellParser [SpellItem]
pItem spelling =
    MP.choice
        [ pure . Punct <$> MP.some (liftP pPunctuation)
        , pure . Number <$> MP.some (liftP pNumber)
        , MP.try (pSyllable spelling)
        , pure <$> pStrayFinal
        , pure . Other <$> MP.some (MP.satisfy isJunk)
        ]

-- | A final mark with no letter in front of it (Mi, ???). A final hangs over
-- the letter it closes, so standing on its own it is no part of any syllable:
-- the reference leaves such a mark exactly as it stands and starts a fresh
-- run at the next letter. So the mark becomes a run of its own - one token,
-- echoed as written, carrying the finding the run records for it. It never
-- swallows the run behind it, which is what separates @Mi@ (M, then ཨི) from
-- a single run of two tokens.
pStrayFinal :: SpellParser SpellItem
pStrayFinal = do
    tok <- MP.satisfy isStrayFinal
    let sp = tokenSpan tok
    pure $
        InvalidSyllableItem
            [tok]
            (findingsDiagnostics sp [FinalWithoutLetter (tokenRaw tok)])

-- | What the otherwise-unrecognized branch may swallow: a token that is
-- neither punctuation (its own branch above) nor the possible start of a
-- syllable run. A junk run ends where a syllable could begin, so the letters
-- after a stray non-Tibetan sign still parse as themselves (ba=la -> བ༴ལ,
-- <ba> -> ༺བ༻) instead of being swallowed whole with it.
isJunk :: Token -> Bool
isJunk token = not (isPunctuationLike token) && not (isSyllableStart token)

-- | A run opens with a token the grammar could work from: a consonant, a
-- subjoined consonant, a vowel, a final, a sign or a Sanskrit mark. Stack
-- breaks, numbers, punctuation and unknown tokens never open one.
isSyllableStart :: Token -> Bool
isSyllableStart token = case tokenCanonical token of
    TcConsonant _ -> True
    TcSubConsonant _ -> True
    TcVowel _ -> True
    TcFinal _ -> True
    TcSign _ -> True
    TcSanskritMark _ -> True
    _ -> False

-- | What may open a run: everything above except a final mark. A final hangs
-- over the letter it closes, so one with nothing in front of it belongs to no
-- syllable - 'pStrayFinal' is the branch that says so and keeps the run
-- behind it whole. A final inside a run is legal and common (kM, oM), which is
-- why the rule is about the head alone.
isSyllableHead :: Token -> Bool
isSyllableHead token = case tokenCanonical token of
    TcFinal _ -> False
    _ -> isSyllableStart token

-- | A final mark standing at the head of what is left to read.
isStrayFinal :: Token -> Bool
isStrayFinal token = case tokenCanonical token of
    TcFinal _ -> True
    _ -> False

-- | The run swallows everything but the boundary: consonants, subjoined
-- forms, vowels, finals, signs, Sanskrit marks, and the stack separators
-- (@.@ and @+@) that glue stacks together. Numbers, half-numbers, unknown
-- tokens and punctuation end the run first.
isSyllableTail :: Token -> Bool
isSyllableTail token = case tokenCanonical token of
    TcConSpec _ -> True
    TcNumber _ -> False
    TcHalfNumber _ -> False
    _ -> isSyllableStart token

-- | One syllable run: the tokens the winning structure claimed, the tail
-- tokens no fresh run would claim, and the boundary stays in the input for
-- the 'Punct' branch to read. The claimed run becomes the nested syllable;
-- signs the run swallowed but could not place are handed on as an
-- 'InvalidSyllableItem' behind it, so no token is lost. The warnings of the
-- constraint windows name the run without the boundary: their span is
-- resolved here from the run's own tokens, once the run is over.
pSyllable :: Spelling -> SpellParser [SpellItem]
pSyllable spelling = do
    start <- MP.getInput
    startOffset <- MP.getOffset
    -- 'lookAhead' only guards the run: the head token must be a possible
    -- syllable start if the run is to be claimed below (numbers, punctuation
    -- and unknown tokens fall through to their own branches), but the head
    -- itself belongs to the structure probe, so nothing may be consumed here.
    void $ MP.lookAhead (MP.satisfy isSyllableHead)
    marked <- MP.option Seq.empty (MP.try (pStructure spelling))
    endOfStructure <- MP.getOffset
    let claimed = endOfStructure - startOffset
    -- A claimed run closes with what the stack above owns; an unclaimed one
    -- keeps swallowing its tail - it is one invalid item by construction and
    -- it has to consume its head, or the sentence would not advance.
    (tailToks, tailMarks) <-
        if claimed == 0
            then do
                toks <- pUnclaimedTail isSyllableTail
                pure (toks, [])
            else stackClose
    findings <- takeFindings
    let content = take claimed start <> tailToks
        markedContent = runItems (toList marked <> tailMarks) content
        runSpan = wordSpan content
        -- A mark at the head of the run with no letter to attach to (wave
        -- 5.4): the output escapes such a character, and this record says
        -- why nothing read it. One record per source character - a
        -- continuation of a decomposed form carries no character of its
        -- own, and the group's own token speaks for it.
        orphans =
            [ SignWithoutLetter (tokenRaw t)
            | (_, t) <- takeWhile (isToplessMark . snd) markedContent
            , not (T.null (tokenRaw t))
            ]
        diags = findingsDiagnostics runSpan (findings <> orphans)
        stacks = withImplicitVowels markedContent (stacksOf (Seq.fromList markedContent))
        -- The signs the run swallowed but no stack took: separators are input
        -- syntax (REFACTOR-AST §8 B2) and the marks the grammar ate - a second
        -- caret, a final the chain refused - live in the findings (Q6), so
        -- what is left is a sign no structure placed.
        leftovers =
            [ t
            | (Nothing, t) <- markedContent
            , not (isConSpecTok t)
            , not (isFinalTok t)
            ]
    pure $
        if claimed == 0
            then [InvalidSyllableItem content diags]
            else
                SyllableItem
                    Syllable
                        { sylStacks = stacks
                        , sylSpan = stacksSpan stacks runSpan
                        , sylDiags = diags
                        }
                    : [InvalidSyllableItem leftovers mempty | not (null leftovers)]
    where
        -- The span of the whole syllable run, from its first to its last
        -- token: the anchor both head warnings quote whole (@tgra@), resolved
        -- here at the end of the run.
        wordSpan :: [Token] -> Span
        wordSpan word@(first : _) =
            Span
                (offsetStart (tokenSpan first))
                (offsetEnd (tokenSpan (foldl (\_ t -> t) first word)))
        wordSpan [] = Span 0 0

        -- What a claimed run owns past its structure: the tail tokens no
        -- fresh run would claim. The reference lets its stack take exactly
        -- one dot after itself (fromWylieOneStack): the first dot of the
        -- tail is that dot - eaten, and this run's single UnplacedDot, a dot
        -- that reaches this close joining no stack to a letter - while a
        -- mark the structures refuse (a Tibetan long a beside its root, a
        -- final or sign after the stack) hangs over the stack it belongs to
        -- and stays in this run, as it did before the tail became a run of
        -- its own. What a fresh run does claim - the letters behind
        -- @....yag@, the @e@ of @ba.e@ - stays in the input for the next
        -- item to read as itself, so a tail that opens a word is not this
        -- run's raw echo.
        stackClose :: SpellParser ([Token], [(Position, Token)])
        stackClose = close False
            where
                -- @dotSeen@: whether the tail's first dot already went past -
                -- only it is eaten and blamed, the dots behind it stay written.
                close :: Bool -> SpellParser ([Token], [(Position, Token)])
                close dotSeen = do
                    next <- MP.lookAhead (MP.optional (liftP pToken))
                    case next of
                        Just tok | isSyllableTail tok -> do
                            fresh <- claimsFreshRun
                            if fresh
                                then pure ([], [])
                                else do
                                    void (liftP pToken)
                                    let isDotTok = isDot tok
                                        eat = not dotSeen && isDotTok
                                    when eat $ noteFinding UnplacedDot
                                    (toks, marks) <- close (dotSeen || isDotTok)
                                    pure
                                        ( tok : toks
                                        , (if eat then [(Eaten, tok)] else []) <> marks
                                        )
                        _ -> pure ([], [])

                -- Whether a run of its own starts here: the grammar decides
                -- what opens a run, not the token class, so the tail is probed
                -- with the real structure choice. The probe reads - pStructure
                -- runs its winner inside 'lookAhead', and a lookAhead rolls
                -- both the input and the scan state back.
                claimsFreshRun :: SpellParser Bool
                claimsFreshRun = do
                    offset <- MP.getOffset
                    after <-
                        MP.optional
                            (MP.try (MP.lookAhead (pStructure spelling *> MP.getOffset)))
                    pure $ maybe False (> offset) after

        -- The marks the grammar emitted keep their emission order and the
        -- unmarked tokens - separators it ate, the unclaimed tail - splice
        -- back into their source positions. The grammar does not always emit
        -- in source order: a caret @^@ written mid-stack is claimed as the
        -- stack's final, so the syllable keeps the emitted order for the
        -- marked letters (the renderers follow it; g^ra is གྲ༹, not ག༹ྲ) and
        -- restores every unmarked token to the gap it stood in.
        runItems :: [(Position, Token)] -> [Token] -> [(Maybe Position, Token)]
        runItems marked run =
            let matched = matchedIndices run marked
                taken = Set.fromList [i | (i, _, _) <- matched]
                unmarked = [(i, t) | (i, t) <- zip [0 ..] run, i `Set.notMember` taken]
             in interleave matched unmarked

        interleave ::
            [(Int, Position, Token)] -> [(Int, Token)] -> [(Maybe Position, Token)]
        interleave [] rest = [(Nothing, t) | (_, t) <- rest]
        interleave ((i, pos, tok) : ms) rest =
            let (before, after) = span (\(j, _) -> j < i) rest
             in [(Nothing, t) | (_, t) <- before]
                    <> [(Just pos, tok)]
                    <> interleave ms after

        -- The source index of every emitted (marked) token: each is found in
        -- the run by its value and the search peers around both sides, so a
        -- reordering (a caret claimed after its stack) still lands on its
        -- physical token, and a letter emitted twice matches its two
        -- occurrences in order.
        matchedIndices :: [Token] -> [(Position, Token)] -> [(Int, Position, Token)]
        matchedIndices run marked = go (zip [0 ..] run) marked []
            where
                go _ [] acc = reverse acc
                go indexeds ((pos, tok) : more) acc =
                    let (before, after) = break ((== tok) . snd) indexeds
                     in case after of
                            (i, _) : rest -> go (before <> rest) more ((i, pos, tok) : acc)
                            -- unreachable: the structures only emit run tokens
                            [] -> reverse acc

-- | The implicit @a@ of a root stack (REFACTOR-AST §4, invariant 5).
--
-- Glyphs write no implicit vowel, so a Unicode-sourced run owes one to every
-- root that carries no written sign; a Wylie-sourced run says in the writing
-- whether it read a vowel-less letter or implicit @a@ (rk is ར + ཀ, rka is
-- རྐ), so its parse already decided and this adds nothing. An a-chen root is
-- a vowel letter of its own (@ཨ@ is spelled @a@) and takes no second @a@.
withImplicitVowels :: [(Maybe Position, Token)] -> Seq Stack -> Seq Stack
withImplicitVowels marked stacks
    | wylieSourced = stacks
    | otherwise = fmap addImplicit stacks
    where
        wylieSourced = case listToMaybe (map snd marked) of
            Just tok -> tokenSource tok == TsWylie
            Nothing -> False
        addImplicit st = case (stRole st, stRoot st, stVowel st) of
            (RoleRoot, Just root, Nothing)
                | writesVowel root -> st{stVowel = Just VowelImplicit}
            _ -> st
        writesVowel root = case tokenCanonical root of
            TcConsonant Ca -> False
            TcConsonant _ -> True
            TcSubConsonant _ -> True
            _ -> False

-- | The span of the stacked signs; a syllable with no stack falls back to the
-- span of its run.
stacksSpan :: Seq Stack -> Span -> Span
stacksSpan stacks fallback = case [tokenSpan t | t <- toList (flatten stacks)] of
    [] -> fallback
    spans -> Span (minimum (map offsetStart spans)) (maximum (map offsetEnd spans))

isConSpecTok :: Token -> Bool
isConSpecTok tok = case tokenCanonical tok of
    TcConSpec _ -> True
    _ -> False

isFinalTok :: Token -> Bool
isFinalTok tok = case tokenCanonical tok of
    TcFinal _ -> True
    _ -> False

-- A syllable must match the structure that consumes the most tokens: a
-- Tibetan syllable extends until the boundary marked by punctuation, exactly
-- as the char-level grammar selected it (e.g. ཕྱི is structure 3, not 1,
-- and པོགས is structure 21, not 17 + 1). The 34 stateless book structures
-- probe as plain parsers in lookahead - no state enters and the pure projection
-- trains no findings; the four stateful ones (8, 9, 21 and the generic word)
-- probe through their pure projection 'runSpell', so a probe can never write
-- findings the real run of the winner would own. The longest match is then run
-- for real. On equal length the earlier structure of 'candidates' wins; that
-- order is a fit to the corpora, not the book's numbering (see 'candidates').
pStructure :: Spelling -> SpellParser TibetanSyllable
pStructure spelling = do
    start <- MP.getOffset
    best <- foldl' better Nothing <$> mapM (probe start) candidates
    case best of
        Nothing -> MP.empty
        Just (_, Stateless p) -> liftP (p spelling)
        Just (_, Stateful p) -> p spelling
    where
        probe start (Stateless p) = probeStateless start p
        probe start (Stateful p) = probeStateful start p
        probeStateless start p = do
            r <-
                MP.option
                    Nothing
                    (Just <$> MP.try (MP.lookAhead ((,) <$> liftP (p spelling) <*> MP.getOffset)))
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (end - start, Stateless p)
        probeStateful start p = do
            r <-
                MP.option
                    Nothing
                    ( Just
                        <$> MP.try (MP.lookAhead ((,) <$> liftP (runSpell (p spelling)) <*> MP.getOffset))
                    )
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (end - start, Stateful p)
        better Nothing c = c
        better c Nothing = c
        better acc@(Just (m, _)) c@(Just (n, _))
            | m >= n = acc
            | otherwise = c

-- | The structures a syllable may be read as, each tagged by whether it keeps
-- spelling state. The order is the tie-break and therefore load-bearing: on an
-- equal match the earlier structure wins.
--
-- The order is a fit to the corpora, not the book's numbering (REFACTOR-AST
-- Q10): on དང the book structures 8 (prefix + root, @dnga@) and 17 (root +
-- suffix, @dang@) both match, and the corpora want @dang@; on དགས they want
-- the prefix reading (9) while on བགས and མངས the root reading (21). No single
-- structural rule satisfies all three, so the tie is lexical: the list keeps
-- the readings the corpora prefer, with the generic word 38 last. Ties the
-- corpora read the other way are logged in @test/parity/known_divergences.tsv@
-- (Q2).
data Candidate
    = Stateless (Spelling -> Parser TibetanSyllable)
    | Stateful (Spelling -> SpellParser TibetanSyllable)

candidates :: [Candidate]
candidates =
    [ Stateless pStructure28
    , Stateless pStructure29
    , Stateless pStructure30
    , Stateless pStructure31
    , Stateless pStructure32
    , Stateless pStructure33
    , Stateless pStructure34
    , Stateless pStructure35
    , Stateless pStructure36
    , Stateless pStructure37
    , Stateful pStructure21
    , Stateless pStructure22
    , Stateless pStructure23
    , Stateless pStructure24
    , Stateless pStructure13
    , Stateless pStructure14
    , Stateless pStructure15
    , Stateless pStructure16
    , Stateless pStructure17
    , Stateless pStructure18
    , Stateless pStructure19
    , Stateless pStructure20
    , Stateful pStructure9
    , Stateless pStructure10
    , Stateless pStructure11
    , Stateless pStructure12
    , Stateless pStructure27
    , Stateless pStructure26
    , Stateless pStructure25
    , Stateless pStructure4
    , Stateless pStructure5
    , Stateless pStructure6
    , Stateless pStructure7
    , Stateful pStructure8
    , Stateless pStructure1
    , Stateless pStructure2
    , Stateless pStructure3
    , Stateful pStructure38
    ]
