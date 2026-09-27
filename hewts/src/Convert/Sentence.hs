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

import Convert.Diagnostic (Diagnostics)
import Convert.Grammar.Legality (checkWord)
import Convert.Grammar.Parser
    ( Parser
    , Spelling (..)
    , isPunctuationLike
    , pNumber
    , pPunctuation
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
    ( ConSpec (..)
    , Token
    , TokenCanonical (..)
    , tokenCanonical
    )
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Text.Megaparsec as MP
import Control.Monad (void)

-- | A syllable as the wave-3.4 dispatcher sees it: the tokens of the whole
-- run up to and including its trailing boundary, each tagged with the place
-- the grammar gave it - 'Nothing' for stack separators the grammar ate
-- without marking and for the boundary itself - plus the spelling warnings
-- 'checkWord' found for the run (without the trailing boundary, so the quoted
-- span still covers exactly what the reference blames).
data Syllable = Syllable
    { syllableTokens :: Seq (Maybe Position, Token)
    , syllableDiags :: Diagnostics
    }
    deriving (Show, Eq)

-- | One piece of a parsed sentence. A recognized run is a 'SyllableItem'; a
-- run that opens like a syllable but that no structure claims is an
-- 'InvalidSyllableItem' (the whole run unmarked, 'syllableDiags' carries what
-- 'checkWord' found for it) - such runs stay visible and diagnosable instead
-- of silently falling into 'Other'.
data SpellItem
    = SyllableItem Syllable
    | InvalidSyllableItem Syllable
    | Number [Token]
    | Punct [Token]
    | Other [Token]
    deriving (Show, Eq)

pSentence :: Spelling -> Parser [SpellItem]
pSentence spelling = MP.many (pItem spelling) <* MP.eof

pItem :: Spelling -> Parser SpellItem
pItem spelling =
    MP.choice
        [ Punct <$> MP.some pPunctuation
        , Number <$> MP.some pNumber
        , MP.try (pSyllable spelling)
        , Other <$> MP.some (MP.satisfy (not . isPunctuationLike))
        ]

-- | One syllable run: a run-starting token, the structure that claims the
-- most of it, everything the run swallows up to the next boundary, and the
-- boundary itself. 'checkWord' sees the run without the boundary.
pSyllable :: Spelling -> Parser SpellItem
pSyllable spelling = do
    start <- MP.getInput
    -- 'lookAhead' only guards the run: the head token must be a possible
    -- syllable start if the run is to be claimed below (numbers, punctuation
    -- and unknown tokens fall through to their own branches), but the head
    -- itself belongs to the structure probe, so nothing may be consumed here.
    void $ MP.lookAhead (MP.satisfy isSyllableStart)
    marked <- MP.option Seq.empty (MP.try (pStructure spelling))
    endOfStructure <- MP.getInput
    let claimed = length start - length endOfStructure
    tailToks <- MP.many (MP.satisfy isSyllableTail)
    boundary <- MP.many pPunctuation
    let content = take claimed start <> tailToks
        syllable =
            Syllable
                { syllableTokens = Seq.fromList (runItems (toList marked) (content <> boundary))
                , syllableDiags = checkWord content
                }
    pure $
        if claimed == 0
            then InvalidSyllableItem syllable
            else SyllableItem syllable
    where
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
                go indexeds [] acc = reverse acc
                go indexeds ((pos, tok) : more) acc =
                    let (before, after) = break ((== tok) . snd) indexeds
                     in case after of
                            (i, _) : rest -> go (before <> rest) more ((i, pos, tok) : acc)
                            -- unreachable: the structures only emit run tokens
                            [] -> reverse acc
        -- A run opens with a token the grammar could work from: a consonant, a
        -- subjoined consonant, a vowel, a final, a sign or a Sanskrit mark.
        -- Stack breaks, numbers, punctuation and unknown tokens never open one.
        isSyllableStart token = case tokenCanonical token of
            TcConsonant _ -> True
            TcSubConsonant _ -> True
            TcVowel _ -> True
            TcFinal _ -> True
            TcSign _ -> True
            TcSanskritMark _ -> True
            _ -> False

        -- The run swallows everything but the boundary: consonants, subjoined
        -- forms, vowels, finals, signs, Sanskrit marks, and the stack
        -- separators (@.@ and @+@) that glue stacks together. Numbers,
        -- half-numbers, unknown tokens and punctuation end the run first.
        isSyllableTail token = case tokenCanonical token of
            TcConSpec _ -> True
            TcNumber _ -> False
            TcHalfNumber _ -> False
            _ -> isSyllableStart token

-- A syllable must match the structure that consumes the most tokens: a
-- Tibetan syllable extends until the boundary marked by punctuation, exactly
-- as the char-level grammar selected it (e.g. ཕྱི is structure 3, not 1,
-- and པོགས is structure 21, not 17 + 1). All structures are probed in
-- lookahead and the one with the longest match is then run for real, so
-- that input is actually consumed.
pStructure :: Spelling -> Parser TibetanSyllable
pStructure spelling = do
    start <- MP.getInput
    let probe p = do
            r <-
                MP.option
                    Nothing
                    (Just <$> MP.try (MP.lookAhead ((,) <$> p <*> MP.getInput)))
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (length start - length end, p)
    best <- foldl' better Nothing <$> mapM probe parses
    maybe MP.empty snd best
    where
        better Nothing c = c
        better c Nothing = c
        better acc@(Just (m, _)) c@(Just (n, _))
            | m >= n = acc
            | otherwise = c
        parses =
            [ pStructure28 spelling
            , pStructure29 spelling
            , pStructure30 spelling
            , pStructure31 spelling
            , pStructure32 spelling
            , pStructure33 spelling
            , pStructure34 spelling
            , pStructure35 spelling
            , pStructure36 spelling
            , pStructure37 spelling
            , pStructure21 spelling
            , pStructure22 spelling
            , pStructure23 spelling
            , pStructure24 spelling
            , pStructure13 spelling
            , pStructure14 spelling
            , pStructure15 spelling
            , pStructure16 spelling
            , pStructure17 spelling
            , pStructure18 spelling
            , pStructure19 spelling
            , pStructure20 spelling
            , pStructure9 spelling
            , pStructure10 spelling
            , pStructure11 spelling
            , pStructure12 spelling
            , pStructure27 spelling
            , pStructure26 spelling
            , pStructure25 spelling
            , pStructure4 spelling
            , pStructure5 spelling
            , pStructure6 spelling
            , pStructure7 spelling
            , pStructure8 spelling
            , pStructure1 spelling
            , pStructure2 spelling
            , pStructure3 spelling
            , pStructure38 spelling
            ]
