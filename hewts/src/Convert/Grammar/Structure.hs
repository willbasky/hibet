{-
	Token-level Tibetan spelling structures.

The 37 syllable structures ('pStructure1' … 'pStructure37'), each composed from
constraint parsers in 'Convert.Grammar.Constraint' and the token-level parser
primitives in 'Convert.Grammar.Parser', and the generic word 'pStructure38',
composed from the same module's generic-word parsers.
Structures recognize a single syllable out of a 'Token' stream; punctuation is
left unconsumed so it survives and is handled separately by 'Convert.Sentence'.
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
    ) where

import qualified Convert.Grammar.Constraint as C
import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token (Token)
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

-- Tibetan spelling structure 1
-- On the basis of the Tibetan spelling grammar 4.1
pStructure1 :: Spelling -> Parser TibetanSyllable
pStructure1 spelling =
    MP.choice
        [ MP.try (C.pConstraint01WithLong spelling)
        , MP.try (C.pConstraint01Sanskrit spelling)
        ]

-- Tibetan spelling structure 2
-- On the basis of the Tibetan spelling grammar 4.8
pStructure2 :: Spelling -> Parser TibetanSyllable
pStructure2 spelling = C.pConstraint08 spelling

-- Tibetan spelling structure 3
-- On the basis of the Tibetan spelling grammar 4.9
pStructure3 :: Spelling -> Parser TibetanSyllable
pStructure3 spelling = C.pConstraint09 spelling

-- Tibetan spelling structure 4
-- On the basis of the Tibetan spelling grammar 4.10
pStructure4 :: Spelling -> Parser TibetanSyllable
pStructure4 spelling = C.pConstraint10 spelling

-- Tibetan spelling structure 5
-- On the basis of the Tibetan spelling grammar 4.11
pStructure5 :: Spelling -> Parser TibetanSyllable
pStructure5 spelling = C.pConstraint11 spelling

-- Tibetan spelling structure 6
-- On the basis of the Tibetan spelling grammar 4.12
pStructure6 :: Spelling -> Parser TibetanSyllable
pStructure6 spelling = C.pConstraint12 spelling

-- Tibetan spelling structure 7
-- On the basis of the Tibetan spelling grammar 4.13
pStructure7 :: Spelling -> Parser TibetanSyllable
pStructure7 spelling = C.pConstraint13 spelling

-- Tibetan spelling structure 8
-- On the basis of the Tibetan spelling grammar 4.14
pStructure8 :: Spelling -> Parser TibetanSyllable
pStructure8 spelling = C.pConstraint14 spelling

-- Tibetan spelling structure 9
-- On the basis of the Tibetan spelling grammar 4.14 and 4.15
pStructure9 :: Spelling -> Parser TibetanSyllable
pStructure9 spelling = C.pConstraint14 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 10
-- On the basis of the Tibetan spelling grammar 4.11 and 4.15
pStructure10 :: Spelling -> Parser TibetanSyllable
pStructure10 spelling = C.pConstraint11 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 11
-- On the basis of the Tibetan spelling grammar 4.12 and 4.15
pStructure11 :: Spelling -> Parser TibetanSyllable
pStructure11 spelling = C.pConstraint12 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 12
-- On the basis of the Tibetan spelling grammar 4.13 and 4.15
pStructure12 :: Spelling -> Parser TibetanSyllable
pStructure12 spelling = C.pConstraint13 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 13
-- On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
pStructure13 :: Spelling -> Parser TibetanSyllable
pStructure13 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint14 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint14 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 14
-- On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
pStructure14 :: Spelling -> Parser TibetanSyllable
pStructure14 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint11 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint11 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 15
-- On the basis of the Tibetan spelling grammar 4.12, 4.15, 4.16
pStructure15 :: Spelling -> Parser TibetanSyllable
pStructure15 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint12 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint12 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 16
-- On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
pStructure16 :: Spelling -> Parser TibetanSyllable
pStructure16 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint13 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint13 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

parseSuffixPostfix ::
    Spelling
    -> Parser TibetanSyllable
    -> Parser TibetanSyllable
    -> Parser Token
    -> Parser TibetanSyllable
parseSuffixPostfix _spelling base suffix post = do
    b <- base
    s <- suffix
    p <- mark Postfix post
    pure (b <> s <> p)

-- Tibetan spelling structure 17
-- On the basis of the Tibetan spelling grammar 4.15
pStructure17 :: Spelling -> Parser TibetanSyllable
pStructure17 = \case
    Tibetan -> do
        root <- mark Root GP.pRootConsonant
        vowel <- MP.optional (mark Vowel GP.pVowel)
        suffix <- C.pConstraint15 Tibetan
        pure (root <> fromMaybe mempty vowel <> suffix)
    Wylie -> do
        root <- mark Root GP.pRootConsonant
        vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
        suffix <- C.pConstraint15 Wylie
        pure (root <> vowel <> suffix)

-- Tibetan spelling structure 18
-- On the basis of the Tibetan spelling grammar 4.8 and 4.15
pStructure18 :: Spelling -> Parser TibetanSyllable
pStructure18 spelling = C.pConstraint08 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 19
-- On the basis of the Tibetan spelling grammar 4.9 and 4.15
pStructure19 :: Spelling -> Parser TibetanSyllable
pStructure19 spelling = C.pConstraint09 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 20
-- On the basis of the Tibetan spelling grammar 4.10 and 4.15
pStructure20 :: Spelling -> Parser TibetanSyllable
pStructure20 spelling = C.pConstraint10 spelling <> C.pConstraint15 spelling

-- Tibetan spelling structure 21
-- On the basis of the Tibetan spelling grammar 4.1, 4.14, 4.15
pStructure21 :: Spelling -> Parser TibetanSyllable
pStructure21 spelling =
    MP.choice
        [ MP.try $ rootSuffixPostfix spelling (C.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ rootSuffixPostfix spelling (C.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 22
-- On the basis of the Tibetan spelling grammar 4.8, 4.14, 4.15
pStructure22 :: Spelling -> Parser TibetanSyllable
pStructure22 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint08 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint08 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 23
-- On the basis of the Tibetan spelling grammar 4.9, 4.14, 4.15
pStructure23 :: Spelling -> Parser TibetanSyllable
pStructure23 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint09 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint09 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

-- Tibetan spelling structure 24
-- On the basis of the Tibetan spelling grammar 4.10, 4.14, 4.15
pStructure24 :: Spelling -> Parser TibetanSyllable
pStructure24 spelling =
    MP.choice
        [ MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint10 spelling)
                (C.pConstraint16Da spelling)
                GP.pPostfixDa
        , MP.try $
            parseSuffixPostfix
                spelling
                (C.pConstraint10 spelling)
                (C.pConstraint16Sa spelling)
                GP.pPostfixSa
        ]

rootSuffixPostfix ::
    Spelling -> Parser TibetanSyllable -> Parser Token -> Parser TibetanSyllable
rootSuffixPostfix spelling suffix post = case spelling of
    Tibetan -> do
        root <- mark Root GP.pRootConsonant
        vowel <- MP.optional (mark Vowel GP.pVowel)
        s <- suffix
        p <- mark Postfix post
        pure (root <> fromMaybe mempty vowel <> s <> p)
    Wylie -> do
        root <- mark Root GP.pRootConsonant
        vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
        s <- suffix
        p <- mark Postfix post
        pure (root <> vowel <> s <> p)

-- Tibetan spelling structure 25
-- On the basis of the Tibetan spelling grammar 4.17
pStructure25 :: Spelling -> Parser TibetanSyllable
pStructure25 spelling =
    MP.choice
        [ MP.try (C.pConstraint17Ra spelling)
        , MP.try (C.pConstraint17Ya spelling)
        ]

-- Tibetan spelling structure 26
-- On the basis of the Tibetan spelling grammar 4.18
pStructure26 :: Spelling -> Parser TibetanSyllable
pStructure26 spelling = C.pConstraint18 spelling

-- Tibetan spelling structure 27
-- On the basis of the Tibetan spelling grammar 4.19
pStructure27 :: Spelling -> Parser TibetanSyllable
pStructure27 spelling = C.pConstraint19 spelling

-- Tibetan spelling structure 28
-- On the basis of the Tibetan spelling grammar 4.1 and 4.20
pStructure28 :: Spelling -> Parser TibetanSyllable
pStructure28 spelling = C.pConstraint01 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 29
-- On the basis of the Tibetan spelling grammar 4.8 and 4.20
pStructure29 :: Spelling -> Parser TibetanSyllable
pStructure29 spelling = C.pConstraint08 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 30
-- On the basis of the Tibetan spelling grammar 4.9 and 4.20
pStructure30 :: Spelling -> Parser TibetanSyllable
pStructure30 spelling = C.pConstraint09 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 31
-- On the basis of the Tibetan spelling grammar 4.10 and 4.20
pStructure31 :: Spelling -> Parser TibetanSyllable
pStructure31 spelling = C.pConstraint10 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 32
-- On the basis of the Tibetan spelling grammar 4.11 and 4.20
pStructure32 :: Spelling -> Parser TibetanSyllable
pStructure32 spelling = C.pConstraint11 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 33
-- On the basis of the Tibetan spelling grammar 4.12 and 4.20
pStructure33 :: Spelling -> Parser TibetanSyllable
pStructure33 spelling = C.pConstraint12 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 34
-- On the basis of the Tibetan spelling grammar 4.13 and 4.20
pStructure34 :: Spelling -> Parser TibetanSyllable
pStructure34 spelling = C.pConstraint13 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 35
-- On the basis of the Tibetan spelling grammar 4.14 and 4.20
pStructure35 :: Spelling -> Parser TibetanSyllable
pStructure35 spelling = C.pConstraint14 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 36
-- On the basis of the Tibetan spelling grammar 4.17 and 4.20
pStructure36 :: Spelling -> Parser TibetanSyllable
pStructure36 spelling =
    ( MP.choice
        [ MP.try (C.pConstraint17Ra spelling)
        , MP.try (C.pConstraint17Ya spelling)
        ]
    )
        <> C.pConstraint20 spelling

-- Tibetan spelling structure 37
-- On the basis of the Tibetan spelling grammar 4.18 and 4.20
pStructure37 :: Spelling -> Parser TibetanSyllable
pStructure37 spelling = C.pConstraint18 spelling <> C.pConstraint20 spelling

-- Tibetan spelling structure 38
-- The generic word: whatever the book structures do not name. Many stacks -
-- consonant-led or vowel-led, with a stack-breaking dot, a lone final mark and
-- a lone subjoined letter as continued words - against one probe (g.yon ->
-- གཡོན, sat+t+wa -> སཏྟྭ). Probed last: a word the book spells keeps the book's
-- marks, a word it does not gets the closest thing the references agree on.
-- Both spellings read identically here: the piece parsers live in
-- 'Convert.Grammar.Constraint' (its generic-word parsers) and decide
-- everything per token, exactly as the old 'Convert.Grammar.Stack' ignored its 'Spelling' argument.
-- A @+@ the stack already closed still reads as a join: the letter after it
-- belongs to the same word (u+e -> ཨེུ), as it does in the references.
pStructure38 :: Spelling -> Parser TibetanSyllable
pStructure38 _spelling = do
    first <- C.pConstraint21First
    rest <- MP.many C.pConstraint21Rest
    pure (first <> mconcat rest)
