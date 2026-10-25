-- | Token-level sentence dispatcher.
--
-- Parses a token stream into a list of 'SpellItem' preserving every token:
-- syllables are recognized via the Convert.Grammar.Structure structures and
-- become the nested 'Convert.Grammar.Syllable.Syllable' of REFACTOR-AST: the
-- grammar itself places every sign into its stack, and the sentence only takes
-- the stacks the winning structure left in the parser state (step 4d).
--
-- A run is one stretch of tokens up to (not including) its boundary: the tsheg
-- or space that closes it is a 'Punct' item of its own (REFACTOR-AST Q4).
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
    , takeStacks
    )
import Convert.Grammar.Structure (claimsFreshRun, pStructure)
import Convert.Grammar.Syllable (Syllable (..))
import Convert.Token
    ( Span (..)
    , Token
    , TokenCanonical (..)
    , isToplessMark
    , offsetEnd
    , offsetStart
    , tokenCanonical
    , tokenRaw
    , tokenSpan
    )
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
pSentence :: Parser [SpellItem]
pSentence = runSpell pSentenceS

-- | The stateful sentence parse: 'StateT ScanState Parser' threads the
-- grammar state across the runs, and each 'pSyllable' takes the findings of
-- the run it owns.
pSentenceS :: SpellParser [SpellItem]
pSentenceS = concat <$> MP.many pItem <* MP.eof

pItem :: SpellParser [SpellItem]
pItem =
    MP.choice
        [ pure . Punct <$> MP.some (liftP pPunctuation)
        , pure . Number <$> MP.some (liftP pNumber)
        , MP.try pSyllable
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
-- the 'Punct' branch to read. The claimed run becomes the nested syllable the
-- grammar built in its state; signs the run swallowed but could not place are
-- handed on as an 'InvalidSyllableItem' behind it, so no token is lost. The
-- warnings of the constraint windows name the run without the boundary: their
-- span is resolved here from the run's own tokens, once the run is over.
pSyllable :: SpellParser [SpellItem]
pSyllable = do
    start <- MP.getInput
    startOffset <- MP.getOffset
    -- A mark at the head of the run with no letter to attach to (wave 5.4):
    -- the output escapes such a character, and this record says why nothing
    -- read it. It is the run's own finding, so it enters the run's findings
    -- here, at the parse - one record per source character: a continuation of
    -- a decomposed form carries no character of its own, and the group's own
    -- token speaks for it.
    let orphans =
            [ SignWithoutLetter (tokenRaw t)
            | t <- takeWhile isToplessMark start
            , not (T.null (tokenRaw t))
            ]
    -- 'lookAhead' only guards the run: the head token must be a possible
    -- syllable start if the run is to be claimed below (numbers, punctuation
    -- and unknown tokens fall through to their own branches), but the head
    -- itself belongs to the structure probe, so nothing may be consumed here.
    void $ MP.lookAhead (MP.satisfy isSyllableHead)
    void $ MP.option mempty (MP.try pStructure)
    endOfStructure <- MP.getOffset
    let claimed = endOfStructure - startOffset
    -- The structure has read the marks the run opened on, so their records
    -- follow the windows of that structure, as the channel always ordered
    -- them.
    mapM_ noteFinding orphans
    -- A claimed run closes with what the stack above owns; an unclaimed one
    -- keeps swallowing its tail - it is one invalid item by construction and
    -- it has to consume its head, or the sentence would not advance.
    tailToks <-
        if claimed == 0
            then pUnclaimedTail isSyllableTail
            else stackClose
    findings <- takeFindings
    built <- takeStacks
    let content = take claimed start <> tailToks
        runSpan = wordSpan content
        diags = findingsDiagnostics runSpan findings
        -- The signs the run swallowed but no stack took: separators are input
        -- syntax (REFACTOR-AST §8 B2) and the marks the grammar ate - a second
        -- caret, a final the chain refused - live in the findings (Q6), so
        -- what is left is a sign no structure placed.
        leftovers = [t | t <- tailToks, not (isConSpecTok t), not (isFinalTok t)]
    pure $
        if claimed == 0
            then [InvalidSyllableItem content diags]
            else
                SyllableItem
                    Syllable
                        { sylStacks = built
                        , sylSpan = runSpan
                        , sylDiags = diags
                        }
                    -- shortcut: the leftovers are part of the run, so they
                    -- carry its warnings and a run with both would report twice;
                    -- in the corpora no run has leftovers, so split the findings
                    -- by element when such input surfaces.
                    : [InvalidSyllableItem leftovers diags | not (null leftovers)]
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
        stackClose :: SpellParser [Token]
        stackClose = close False
            where
                -- @dotSeen@: whether the tail's first dot already went past -
                -- only it is eaten and blamed, the dots behind it stay written.
                close :: Bool -> SpellParser [Token]
                close dotSeen = do
                    next <- MP.lookAhead (MP.optional (liftP pToken))
                    case next of
                        Just tok | isSyllableTail tok -> do
                            fresh <- claimsFreshRun
                            if fresh
                                then pure []
                                else do
                                    void (liftP pToken)
                                    let isDotTok = isDot tok
                                        eat = not dotSeen && isDotTok
                                    when eat $ noteFinding UnplacedDot
                                    toks <- close (dotSeen || isDotTok)
                                    pure (tok : toks)
                        _ -> pure []

isConSpecTok :: Token -> Bool
isConSpecTok tok = case tokenCanonical tok of
    TcConSpec _ -> True
    _ -> False

isFinalTok :: Token -> Bool
isFinalTok tok = case tokenCanonical tok of
    TcFinal _ -> True
    _ -> False
