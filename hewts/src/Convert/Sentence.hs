-- | Token-level sentence dispatcher.
--
-- Parses a token stream into a list of 'SpellItem' preserving every token:
-- syllables are recognized via the Convert.Grammar.Structure structures,
-- Tibetan digits ༠–༩ become 'Number', punctuation and whitespace tokens are
-- kept as 'Punct', and anything unrecognized is kept as 'Other' (nothing is
-- dropped).
--
-- A syllable is one run of tokens up to and including its trailing boundary
-- (whitespace or punctuation): the whole run travels as the item (wave 3.4.2).
module Convert.Sentence
    ( SpellItem (..)
    , Syllable (..)
    , pSentence
    ) where

import Control.Monad (void)
import Convert.Diagnostic
    ( Diagnostics
    , Finding (FinalWithoutLetter)
    , findingsDiagnostics
    )
import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , Spelling (..)
    , isPunctuationLike
    , liftP
    , pNumber
    , pPunctuation
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
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token
    ( Span (..)
    , Token
    , TokenCanonical (..)
    , offsetEnd
    , offsetStart
    , tokenCanonical
    , tokenRaw
    , tokenSpan
    )
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Text.Megaparsec as MP

-- | A syllable as the wave-3.4 dispatcher sees it: the tokens of the whole
-- run up to and including its trailing boundary, each tagged with the place
-- the grammar gave it - 'Nothing' for stack separators the grammar ate
-- without marking and for the boundary itself - plus the spelling warnings
-- the grammar's constraint windows recorded for the run (without the
-- trailing boundary, so the quoted span still covers exactly what the
-- warning blames).
data Syllable = Syllable
    { syllableTokens :: Seq (Maybe Position, Token)
    , syllableDiags :: Diagnostics
    }
    deriving (Show, Eq)

-- | One piece of a parsed sentence. A recognized run is a 'SyllableItem'; a
-- run that opens like a syllable but that no structure claims is an
-- 'InvalidSyllableItem' (the whole run unmarked; with no winning structure no
-- constraint window was hosted, so no warning fires for it) - such runs stay
-- visible and diagnosable instead of silently falling into 'Other'.
data SpellItem
    = SyllableItem Syllable
    | InvalidSyllableItem Syllable
    | Number [Token]
    | Punct [Token]
    | Other [Token]
    deriving (Show, Eq)

-- | The sentence parse as a plain token parse: the stateful grammar
-- ('pSentenceS') runs with a fresh state and the state is discarded - the
-- warnings the constraint windows produced are already spelled into each
-- run's 'syllableDiags' - so the entry point stays as the pure 'Parser' the
-- converter and the tests drive.
pSentence :: Spelling -> Parser [SpellItem]
pSentence spelling = runSpell (pSentenceS spelling)

-- | The stateful sentence parse: 'StateT ScanState Parser' threads the
-- grammar state across the runs, and each 'pSyllable' takes the findings of
-- the run it owns.
pSentenceS :: Spelling -> SpellParser [SpellItem]
pSentenceS spelling = MP.many (pItem spelling) <* MP.eof

pItem :: Spelling -> SpellParser SpellItem
pItem spelling =
    MP.choice
        [ Punct <$> MP.some (liftP pPunctuation)
        , Number <$> MP.some (liftP pNumber)
        , MP.try (pSyllable spelling)
        , pStrayFinal
        , Other <$> MP.some (MP.satisfy (not . isPunctuationLike))
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
            Syllable
                { syllableTokens = Seq.fromList [(Nothing, tok)]
                , syllableDiags =
                    findingsDiagnostics sp [FinalWithoutLetter (tokenRaw tok)]
                }

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

-- | One syllable run: a run-starting token, the structure that claims the
-- most of it, everything the run swallows up to the next boundary, and the
-- boundary itself. The warnings of the constraint windows name the run
-- without the boundary: their span is resolved here from the run's own
-- tokens, once the run is over.
pSyllable :: Spelling -> SpellParser SpellItem
pSyllable spelling = do
    start <- MP.getInput
    -- 'lookAhead' only guards the run: the head token must be a possible
    -- syllable start if the run is to be claimed below (numbers, punctuation
    -- and unknown tokens fall through to their own branches), but the head
    -- itself belongs to the structure probe, so nothing may be consumed here.
    void $ MP.lookAhead (MP.satisfy isSyllableHead)
    marked <- MP.option Seq.empty (MP.try (pStructure spelling))
    endOfStructure <- MP.getInput
    let claimed = length start - length endOfStructure
    tailToks <- pUnclaimedTail isSyllableTail
    boundary <- MP.many (liftP pPunctuation)
    findings <- takeFindings
    let content = take claimed start <> tailToks
        markedContent = runItems (toList marked) content
        runSpan = wordSpan content
        syllable =
            Syllable
                { syllableTokens = Seq.fromList (markedContent <> [(Nothing, t) | t <- boundary])
                , syllableDiags = findingsDiagnostics runSpan findings
                }
    pure $
        if claimed == 0
            then InvalidSyllableItem syllable
            else SyllableItem syllable
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

        -- The marks the grammar emitted keep their emission order and the
        -- unmarked tokens - separators it ate, the unclaimed tail, the
        -- boundary - splice back into their source positions. The grammar
        -- does not always emit in source order: a caret @^@ written mid-stack
        -- is claimed as the stack's final, so the syllable keeps the emitted
        -- order for the marked letters (the renderers follow it; g^ra is
        -- གྲ༹, not ག༹ྲ) and restores every unmarked token to the gap it
        -- stood in.
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

-- A syllable must match the structure that consumes the most tokens: a
-- Tibetan syllable extends until the boundary marked by punctuation, exactly
-- as the char-level grammar selected it (e.g. ཕྱི is structure 3, not 1,
-- and པོགས is structure 21, not 17 + 1). The 34 stateless book structures
-- probe as plain parsers in lookahead - no state enters and the pure projection
-- trains no findings; the four stateful ones (8, 9, 21 and the generic word)
-- probe through their pure projection 'runSpell', so a probe can never write
-- findings the real run of the winner would own. The longest match is then run
-- for real. On equal length the earlier (book) structure wins, exactly as the
-- book's order did - the two kinds are therefore probed in one book-ordered
-- list, not one kind after the other.
pStructure :: Spelling -> SpellParser TibetanSyllable
pStructure spelling = do
    start <- MP.getInput
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
                    (Just <$> MP.try (MP.lookAhead ((,) <$> liftP (p spelling) <*> MP.getInput)))
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (length start - length end, Stateless p)
        probeStateful start p = do
            r <-
                MP.option
                    Nothing
                    ( Just
                        <$> MP.try (MP.lookAhead ((,) <$> liftP (runSpell (p spelling)) <*> MP.getInput))
                    )
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (length start - length end, Stateful p)
        better Nothing c = c
        better c Nothing = c
        better acc@(Just (m, _)) c@(Just (n, _))
            | m >= n = acc
            | otherwise = c

-- | The structures a syllable may be read as, each tagged by whether it keeps
-- spelling state, in the book's own order. The order is the tie-break and
-- therefore load-bearing: on an equal match the earlier structure wins, and the
-- list is kept exactly as the book orders the shapes.
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
