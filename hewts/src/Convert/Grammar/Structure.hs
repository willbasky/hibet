{-
	Token-level Tibetan spelling structures.

The 37 syllable structures ('pStructure1' … 'pStructure37'), each composed from
constraint parsers in 'Convert.Grammar.Constraint' and the token-level parser
primitives in 'Convert.Grammar.Parser', and the generic word 'pStructure38',
composed from the same module's generic-word parsers. Every structure is a spell
parser: placing a letter writes it into the stack it belongs to (REFACTOR-AST
step 4d), and the marks the rule emits are what the windows read. Three of them
(8, 9, 21) host the ambiguous-spelling window at their tail ('spellAs'), and the
generic word carries the spelling state of the word tail.
Structures recognize a single syllable out of a 'Token' stream; punctuation is
left unconsumed so it survives and is handled separately by 'Convert.Sentence'.
Which structure reads a run is decided here too ('pStructure'): the longest
match wins, and 'candidates' is the tie-break order.
-}

module Convert.Grammar.Structure
    ( pStructure1
    , pStructure2
    , pStructure3
    , pStructure4
    , pStructure5
    , pStructure6
    , pStructure7
    , pStructure8
    , pStructure9
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
    , pStructure30
    , pStructure31
    , pStructure32
    , pStructure33
    , pStructure34
    , pStructure35
    , pStructure36
    , pStructure37
    , pStructure38
    , pStructure
    , claimsFreshRun
    ) where

import qualified Convert.Grammar.Constraint as C
import Convert.Grammar.Parser (Parser, SpellParser, liftP, mark, vowelSlot)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
import Convert.Token (Token)
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

-- Tibetan spelling structure 1
-- On the basis of the Tibetan spelling grammar 4.1
pStructure1 :: SpellParser TibetanSyllable
pStructure1 =
    MP.choice
        [ MP.try C.pConstraint01WithLong
        , MP.try C.pConstraint01Sanskrit
        ]

-- Tibetan spelling structure 2
-- On the basis of the Tibetan spelling grammar 4.8
pStructure2 :: SpellParser TibetanSyllable
pStructure2 = C.pConstraint08

-- Tibetan spelling structure 3
-- On the basis of the Tibetan spelling grammar 4.9
pStructure3 :: SpellParser TibetanSyllable
pStructure3 = C.pConstraint09

-- Tibetan spelling structure 4
-- On the basis of the Tibetan spelling grammar 4.10
pStructure4 :: SpellParser TibetanSyllable
pStructure4 = C.pConstraint10

-- Tibetan spelling structure 5
-- On the basis of the Tibetan spelling grammar 4.11
pStructure5 :: SpellParser TibetanSyllable
pStructure5 = C.pConstraint11

-- Tibetan spelling structure 6
-- On the basis of the Tibetan spelling grammar 4.12
pStructure6 :: SpellParser TibetanSyllable
pStructure6 = C.pConstraint12

-- Tibetan spelling structure 7
-- On the basis of the Tibetan spelling grammar 4.13
pStructure7 :: SpellParser TibetanSyllable
pStructure7 = C.pConstraint13

-- Tibetan spelling structure 8
-- On the basis of the Tibetan spelling grammar 4.14
--
-- The first of the three structures that host a window: the syllable is
-- finished the moment rule 4.14 has run, and only the finished syllable says
-- which of its two readings won, so the ambiguous-spelling window sits here
-- ('Convert.Grammar.Constraint.Constraint22').
pStructure8 :: SpellParser TibetanSyllable
pStructure8 = spellAs C.pConstraint14

-- Tibetan spelling structure 9
-- On the basis of the Tibetan spelling grammar 4.14 and 4.15
pStructure9 :: SpellParser TibetanSyllable
pStructure9 = spellAs (C.pConstraint14 `seqJoinS` C.pConstraint15)

-- Tibetan spelling structure 10
-- On the basis of the Tibetan spelling grammar 4.11 and 4.15
pStructure10 :: SpellParser TibetanSyllable
pStructure10 = C.pConstraint11 `seqJoinS` C.pConstraint15

-- Tibetan spelling structure 11
-- On the basis of the Tibetan spelling grammar 4.12 and 4.15
pStructure11 :: SpellParser TibetanSyllable
pStructure11 = C.pConstraint12 `seqJoinS` C.pConstraint15

-- Tibetan spelling structure 12
-- On the basis of the Tibetan spelling grammar 4.13 and 4.15
pStructure12 :: SpellParser TibetanSyllable
pStructure12 = C.pConstraint13 `seqJoinS` C.pConstraint15

-- Tibetan spelling structure 13
-- On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
pStructure13 :: SpellParser TibetanSyllable
pStructure13 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint14
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint14
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 14
-- On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
pStructure14 :: SpellParser TibetanSyllable
pStructure14 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint11
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint11
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 15
-- On the basis of the Tibetan spelling grammar 4.12, 4.15, 4.16
pStructure15 :: SpellParser TibetanSyllable
pStructure15 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint12
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint12
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 16
-- On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
pStructure16 :: SpellParser TibetanSyllable
pStructure16 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint13
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint13
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

parseSuffixPostfix ::
    SpellParser TibetanSyllable
    -> SpellParser TibetanSyllable
    -> Parser Token
    -> SpellParser TibetanSyllable
parseSuffixPostfix base suffix post = do
    b <- base
    s <- suffix
    p <- mark Postfix post
    pure (b <> s <> p)

-- | Run the structure and hold the window at its tail.
--
-- The three structures that host a window are the only book structures that
-- need one, and the only thing the window spends is the finding it records: the
-- syllables of the completed rule are already placed, and the window reads the
-- marks it emitted. The window therefore runs where the syllable ends - after
-- its last letter is placed - and not anywhere earlier, which for a composed
-- structure is not the same moment.
spellAs :: SpellParser TibetanSyllable -> SpellParser TibetanSyllable
spellAs structure = do
    syllable <- structure
    C.noteAmbiguous
    pure syllable

-- | Two structure parsers one after the other, their syllables joined: the
-- book stacks the two rules into one word (4.14 then 4.15 -> བཏག), and the
-- parse result has no 'Semigroup' instance, so the join the pure parser used to
-- get through 'Applicative' is spelled out here. Only the returned marks are
-- joined; the stacks are already in the parser state, in the order the letters
-- were placed.
seqJoinS ::
    SpellParser TibetanSyllable
    -> SpellParser TibetanSyllable
    -> SpellParser TibetanSyllable
seqJoinS p q = do
    a <- p
    b <- q
    pure (a <> b)

-- Tibetan spelling structure 17
-- On the basis of the Tibetan spelling grammar 4.15
pStructure17 :: SpellParser TibetanSyllable
pStructure17 = do
    root <- mark Root GP.pRootConsonant
    vowel <- vowelSlot root [mark Vowel GP.pVowel]
    suffix <- C.pConstraint15
    pure (root <> vowel <> suffix)

-- Tibetan spelling structure 18
-- On the basis of the Tibetan spelling grammar 4.8 and 4.15
pStructure18 :: SpellParser TibetanSyllable
pStructure18 = C.pConstraint08 `seqJoinS` C.pConstraint15

-- Tibetan spelling structure 19
-- On the basis of the Tibetan spelling grammar 4.9 and 4.15
pStructure19 :: SpellParser TibetanSyllable
pStructure19 = C.pConstraint09 `seqJoinS` C.pConstraint15

-- Tibetan spelling structure 20
-- On the basis of the Tibetan spelling grammar 4.10 and 4.15
pStructure20 :: SpellParser TibetanSyllable
pStructure20 = C.pConstraint10 `seqJoinS` C.pConstraint15

-- Tibetan spelling structure 21
-- On the basis of the Tibetan spelling grammar 4.1, 4.14, 4.15
--
-- The third structure that hosts a window, and the one that reads the
-- three-letter forms the prefix-free way: the postfix is the syllable's last
-- letter here, so the window runs after it is placed.
pStructure21 :: SpellParser TibetanSyllable
pStructure21 =
    spellAs $
        MP.choice
            [ MP.try $ rootSuffixPostfix C.pConstraint16Da GP.pPostfixDa
            , MP.try $ rootSuffixPostfix C.pConstraint16Sa GP.pPostfixSa
            ]

-- Tibetan spelling structure 22
-- On the basis of the Tibetan spelling grammar 4.8, 4.14, 4.15
pStructure22 :: SpellParser TibetanSyllable
pStructure22 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint08
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint08
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 23
-- On the basis of the Tibetan spelling grammar 4.9, 4.14, 4.15
pStructure23 :: SpellParser TibetanSyllable
pStructure23 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint09
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint09
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 24
-- On the basis of the Tibetan spelling grammar 4.10, 4.14, 4.15
pStructure24 :: SpellParser TibetanSyllable
pStructure24 =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                C.pConstraint10
                C.pConstraint16Da
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                C.pConstraint10
                C.pConstraint16Sa
                GP.pPostfixSa
        ]

rootSuffixPostfix ::
    SpellParser TibetanSyllable
    -> Parser Token
    -> SpellParser TibetanSyllable
rootSuffixPostfix suffix post = do
    root <- mark Root GP.pRootConsonant
    vowel <- vowelSlot root [mark Vowel GP.pVowel]
    s <- suffix
    p <- mark Postfix post
    pure (root <> vowel <> s <> p)

-- Tibetan spelling structure 25
-- On the basis of the Tibetan spelling grammar 4.17
pStructure25 :: SpellParser TibetanSyllable
pStructure25 =
    MP.choice
        [ MP.try C.pConstraint17Ra
        , MP.try C.pConstraint17Ya
        ]

-- Tibetan spelling structure 26
-- On the basis of the Tibetan spelling grammar 4.18
pStructure26 :: SpellParser TibetanSyllable
pStructure26 = C.pConstraint18

-- Tibetan spelling structure 27
-- On the basis of the Tibetan spelling grammar 4.19
pStructure27 :: SpellParser TibetanSyllable
pStructure27 = C.pConstraint19

-- Tibetan spelling structure 28
-- On the basis of the Tibetan spelling grammar 4.1 and 4.20
pStructure28 :: SpellParser TibetanSyllable
pStructure28 = C.pConstraint01 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 29
-- On the basis of the Tibetan spelling grammar 4.8 and 4.20
pStructure29 :: SpellParser TibetanSyllable
pStructure29 = C.pConstraint08 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 30
-- On the basis of the Tibetan spelling grammar 4.9 and 4.20
pStructure30 :: SpellParser TibetanSyllable
pStructure30 = C.pConstraint09 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 31
-- On the basis of the Tibetan spelling grammar 4.10 and 4.20
pStructure31 :: SpellParser TibetanSyllable
pStructure31 = C.pConstraint10 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 32
-- On the basis of the Tibetan spelling grammar 4.11 and 4.20
pStructure32 :: SpellParser TibetanSyllable
pStructure32 = C.pConstraint11 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 33
-- On the basis of the Tibetan spelling grammar 4.12 and 4.20
pStructure33 :: SpellParser TibetanSyllable
pStructure33 = C.pConstraint12 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 34
-- On the basis of the Tibetan spelling grammar 4.13 and 4.20
pStructure34 :: SpellParser TibetanSyllable
pStructure34 = C.pConstraint13 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 35
-- On the basis of the Tibetan spelling grammar 4.14 and 4.20
pStructure35 :: SpellParser TibetanSyllable
pStructure35 = C.pConstraint14 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 36
-- On the basis of the Tibetan spelling grammar 4.17 and 4.20
pStructure36 :: SpellParser TibetanSyllable
pStructure36 =
    ( MP.choice
        [ MP.try C.pConstraint17Ra
        , MP.try C.pConstraint17Ya
        ]
    )
        `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 37
-- On the basis of the Tibetan spelling grammar 4.18 and 4.20
pStructure37 :: SpellParser TibetanSyllable
pStructure37 = C.pConstraint18 `seqJoinS` C.pConstraint20

-- Tibetan spelling structure 38
-- The generic word: whatever the book structures do not name. Many stacks -
-- consonant-led or vowel-led, with a stack-breaking dot, a lone final mark and
-- a lone subjoined letter as continued words - against one probe (g.yon ->
-- གཡོན, sat+t+wa -> སཏྟྭ). Probed last: a word the book spells keeps the book's
-- marks, a word it does not gets the closest thing the references agree on.
-- Both spellings read identically here: the piece parsers live in
-- 'Convert.Grammar.Constraint' (its generic-word parsers) and decide
-- everything per token, exactly as the old 'Convert.Grammar.Stack' ignored the spelling it was handed.
-- A @+@ the stack already closed still reads as a join: the letter after it
-- belongs to the same word (u+e -> ཨེུ), as it does in the references.
-- The only structure whose legality windows accumulate findings: the rules
-- below it in 'Convert.Grammar.Constraint.Constraint21' record them, so its
-- probe runs through the pure projection and the real run owns the findings.
pStructure38 :: SpellParser TibetanSyllable
pStructure38 = do
    first <- C.pConstraint21First
    rest <- MP.many C.pConstraint21Rest
    pure (first <> mconcat rest)

-- | A syllable must match the structure that consumes the most tokens: a
-- Tibetan syllable extends until the boundary marked by punctuation, exactly
-- as the char-level grammar selected it (e.g. ཕྱི is structure 3, not 1,
-- and པོགས is structure 21, not 17 + 1). Every probe runs its structure in the
-- pure projection 'GP.runSpell', so a probe builds no stacks and records no
-- findings the real run of the winner would own; the winner then runs for real
-- and its placements are the syllable. The longest match wins. On equal length
-- the earlier structure of 'candidates' wins; that order is a fit to the
-- corpora, not the book's numbering (see 'candidates').
pStructure :: SpellParser TibetanSyllable
pStructure = do
    start <- MP.getOffset
    best <- foldl' better Nothing <$> mapM (probe start) candidates
    case best of
        Nothing -> MP.empty
        Just (_, p) -> p
    where
        probe start p = do
            r <-
                MP.option
                    Nothing
                    ( Just
                        <$> MP.try
                            (MP.lookAhead ((,) <$> liftP (GP.runSpell p) <*> MP.getOffset))
                    )
            pure $ case r of
                Nothing -> Nothing
                Just (_, end) -> Just (end - start, p)
        better Nothing c = c
        better c Nothing = c
        better acc@(Just (m, _)) c@(Just (n, _))
            | m >= n = acc
            | otherwise = c

-- | Whether a run of its own starts at the current input: the grammar decides
-- what opens a run, not the token class, so the tail is probed with the real
-- structure choice. The probe runs in the pure projection, so neither the
-- tokens nor the stacks of the word being read are touched.
claimsFreshRun :: SpellParser Bool
claimsFreshRun = do
    offset <- MP.getOffset
    after <-
        MP.optional
            ( MP.try
                ( MP.lookAhead
                    (liftP (GP.runSpell (pStructure)) *> MP.getOffset)
                )
            )
    pure $ maybe False (> offset) after

-- | The structures a syllable may be read as. The order is the tie-break and
-- therefore load-bearing: on an equal match the earlier structure wins.
--
-- The order is a fit to the corpora, not the book's numbering (REFACTOR-AST
-- Q10): on དང the book structures 8 (prefix + root, @dnga@) and 17 (root +
-- suffix, @dang@) both match, and the corpora want @dang@; on དགས they want
-- the prefix reading (9) while on བགས and མངས the root reading (21). No single
-- structural rule satisfies all three, so the tie is lexical: the list keeps
-- the readings the corpora prefer, with the generic word 38 last. Ties the
-- corpora read the other way are logged in @test/parity/known_divergences.tsv@
-- (Q2).
candidates :: [SpellParser TibetanSyllable]
candidates =
    [ pStructure28
    , pStructure29
    , pStructure30
    , pStructure31
    , pStructure32
    , pStructure33
    , pStructure34
    , pStructure35
    , pStructure36
    , pStructure37
    , pStructure21
    , pStructure22
    , pStructure23
    , pStructure24
    , pStructure13
    , pStructure14
    , pStructure15
    , pStructure16
    , pStructure17
    , pStructure18
    , pStructure19
    , pStructure20
    , pStructure9
    , pStructure10
    , pStructure11
    , pStructure12
    , pStructure27
    , pStructure26
    , pStructure25
    , pStructure4
    , pStructure5
    , pStructure6
    , pStructure7
    , pStructure8
    , pStructure1
    , pStructure2
    , pStructure3
    , pStructure38
    ]
