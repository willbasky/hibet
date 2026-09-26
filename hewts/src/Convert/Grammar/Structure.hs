{-
	Token-level Tibetan spelling structures.

The 37 syllable structures ('pStructure1' … 'pStructure37'), each composed from
constraint parsers in 'Convert.Grammar.Constraint' and the token-level parser
primitives in 'Convert.Grammar.Parser'. Structures recognize a single syllable
out of a 'Token' stream; punctuation is left unconsumed so it survives and is
handled separately by 'Convert.Sentence'.
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
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import qualified Convert.Grammar.Constraint.Constraint01 as C01
import qualified Convert.Grammar.Constraint.Constraint08 as C08
import qualified Convert.Grammar.Constraint.Constraint09 as C09
import qualified Convert.Grammar.Constraint.Constraint10 as C10
import qualified Convert.Grammar.Constraint.Constraint11 as C11
import qualified Convert.Grammar.Constraint.Constraint12 as C12
import qualified Convert.Grammar.Constraint.Constraint13 as C13
import qualified Convert.Grammar.Constraint.Constraint14 as C14
import qualified Convert.Grammar.Constraint.Constraint15 as C15
import qualified Convert.Grammar.Constraint.Constraint16 as C16
import qualified Convert.Grammar.Constraint.Constraint17 as C17
import qualified Convert.Grammar.Constraint.Constraint18 as C18
import qualified Convert.Grammar.Constraint.Constraint19 as C19
import qualified Convert.Grammar.Constraint.Constraint20 as C20
import Convert.Token (Token)
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

-- Tibetan spelling structure 1
-- On the basis of the Tibetan spelling grammar 4.1
pStructure1 :: Spelling -> Parser TibetanWord
pStructure1 spelling = MP.choice
    [ MP.try (C01.pConstraint01WithLong spelling)
    , MP.try (C01.pConstraint01Sanskrit spelling)
    ]

-- Tibetan spelling structure 2
-- On the basis of the Tibetan spelling grammar 4.8
pStructure2 :: Spelling -> Parser TibetanWord
pStructure2 spelling = C08.pConstraint08 spelling

-- Tibetan spelling structure 3
-- On the basis of the Tibetan spelling grammar 4.9
pStructure3 :: Spelling -> Parser TibetanWord
pStructure3 spelling = C09.pConstraint09 spelling

-- Tibetan spelling structure 4
-- On the basis of the Tibetan spelling grammar 4.10
pStructure4 :: Spelling -> Parser TibetanWord
pStructure4 spelling = C10.pConstraint10 spelling

-- Tibetan spelling structure 5
-- On the basis of the Tibetan spelling grammar 4.11
pStructure5 :: Spelling -> Parser TibetanWord
pStructure5 spelling = C11.pConstraint11 spelling

-- Tibetan spelling structure 6
-- On the basis of the Tibetan spelling grammar 4.12
pStructure6 :: Spelling -> Parser TibetanWord
pStructure6 spelling = C12.pConstraint12 spelling

-- Tibetan spelling structure 7
-- On the basis of the Tibetan spelling grammar 4.13
pStructure7 :: Spelling -> Parser TibetanWord
pStructure7 spelling = C13.pConstraint13 spelling

-- Tibetan spelling structure 8
-- On the basis of the Tibetan spelling grammar 4.14
pStructure8 :: Spelling -> Parser TibetanWord
pStructure8 spelling = C14.pConstraint14 spelling

-- Tibetan spelling structure 9
-- On the basis of the Tibetan spelling grammar 4.14 and 4.15
pStructure9 :: Spelling -> Parser TibetanWord
pStructure9 spelling = C14.pConstraint14 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 10
-- On the basis of the Tibetan spelling grammar 4.11 and 4.15
pStructure10 :: Spelling -> Parser TibetanWord
pStructure10 spelling = C11.pConstraint11 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 11
-- On the basis of the Tibetan spelling grammar 4.12 and 4.15
pStructure11 :: Spelling -> Parser TibetanWord
pStructure11 spelling = C12.pConstraint12 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 12
-- On the basis of the Tibetan spelling grammar 4.13 and 4.15
pStructure12 :: Spelling -> Parser TibetanWord
pStructure12 spelling = C13.pConstraint13 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 13
-- On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
pStructure13 :: Spelling -> Parser TibetanWord
pStructure13 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C14.pConstraint14 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C14.pConstraint14 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 14
-- On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
pStructure14 :: Spelling -> Parser TibetanWord
pStructure14 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C11.pConstraint11 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C11.pConstraint11 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 15
-- On the basis of the Tibetan spelling grammar 4.12, 4.15, 4.16
pStructure15 :: Spelling -> Parser TibetanWord
pStructure15 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C12.pConstraint12 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C12.pConstraint12 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 16
-- On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
pStructure16 :: Spelling -> Parser TibetanWord
pStructure16 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C13.pConstraint13 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C13.pConstraint13 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

parseSuffixPostfix :: Spelling -> Parser TibetanWord -> Parser TibetanWord -> Parser Token -> Parser TibetanWord
parseSuffixPostfix spelling base suffix post = do
    b <- base
    s <- suffix
    p <- mark Postfix post
    pure (b <> s <> p)

-- Tibetan spelling structure 17
-- On the basis of the Tibetan spelling grammar 4.15
pStructure17 :: Spelling -> Parser TibetanWord
pStructure17 spelling = do
    root <- mark Root GP.pRootConsonant
    vowel <- MP.optional (vowelSlot spelling)
    suffix <- C15.pConstraint15 spelling
    pure (root <> fromMaybe mempty vowel <> suffix)

-- Tibetan spelling structure 18
-- On the basis of the Tibetan spelling grammar 4.8 and 4.15
pStructure18 :: Spelling -> Parser TibetanWord
pStructure18 spelling = C08.pConstraint08 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 19
-- On the basis of the Tibetan spelling grammar 4.9 and 4.15
pStructure19 :: Spelling -> Parser TibetanWord
pStructure19 spelling = C09.pConstraint09 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 20
-- On the basis of the Tibetan spelling grammar 4.10 and 4.15
pStructure20 :: Spelling -> Parser TibetanWord
pStructure20 spelling = C10.pConstraint10 spelling <> C15.pConstraint15 spelling

-- Tibetan spelling structure 21
-- On the basis of the Tibetan spelling grammar 4.1, 4.14, 4.15
pStructure21 :: Spelling -> Parser TibetanWord
pStructure21 spelling =
    MP.choice
        [ MP.try $ rootSuffixPostfix spelling (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ rootSuffixPostfix spelling (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 22
-- On the basis of the Tibetan spelling grammar 4.8, 4.14, 4.15
pStructure22 :: Spelling -> Parser TibetanWord
pStructure22 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C08.pConstraint08 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C08.pConstraint08 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 23
-- On the basis of the Tibetan spelling grammar 4.9, 4.14, 4.15
pStructure23 :: Spelling -> Parser TibetanWord
pStructure23 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C09.pConstraint09 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C09.pConstraint09 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

-- Tibetan spelling structure 24
-- On the basis of the Tibetan spelling grammar 4.10, 4.14, 4.15
pStructure24 :: Spelling -> Parser TibetanWord
pStructure24 spelling =
    MP.choice
        [ MP.try $ parseSuffixPostfix spelling (C10.pConstraint10 spelling) (C16.pConstraint16Da spelling) GP.pPostfixDa
        , MP.try $ parseSuffixPostfix spelling (C10.pConstraint10 spelling) (C16.pConstraint16Sa spelling) GP.pPostfixSa
        ]

rootSuffixPostfix :: Spelling -> Parser TibetanWord -> Parser Token -> Parser TibetanWord
rootSuffixPostfix spelling suffix  post = do
    root <- mark Root GP.pRootConsonant
    vowel <- MP.optional (vowelSlot spelling)
    s <- suffix
    p <- mark Postfix post
    pure (root <> fromMaybe mempty vowel <> s <> p)

-- Tibetan spelling structure 25
-- On the basis of the Tibetan spelling grammar 4.17
pStructure25 :: Spelling -> Parser TibetanWord
pStructure25 spelling =
    MP.choice
        [ MP.try (C17.pConstraint17Ra spelling)
        , MP.try (C17.pConstraint17Ya spelling)
        ]

-- Tibetan spelling structure 26
-- On the basis of the Tibetan spelling grammar 4.18
pStructure26 :: Spelling -> Parser TibetanWord
pStructure26 spelling = C18.pConstraint18 spelling

-- Tibetan spelling structure 27
-- On the basis of the Tibetan spelling grammar 4.19
pStructure27 :: Spelling -> Parser TibetanWord
pStructure27 spelling = C19.pConstraint19 spelling

-- Tibetan spelling structure 28
-- On the basis of the Tibetan spelling grammar 4.1 and 4.20
pStructure28 :: Spelling -> Parser TibetanWord
pStructure28 spelling = C01.pConstraint01 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 29
-- On the basis of the Tibetan spelling grammar 4.8 and 4.20
pStructure29 :: Spelling -> Parser TibetanWord
pStructure29 spelling = C08.pConstraint08 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 30
-- On the basis of the Tibetan spelling grammar 4.9 and 4.20
pStructure30 :: Spelling -> Parser TibetanWord
pStructure30 spelling = C09.pConstraint09 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 31
-- On the basis of the Tibetan spelling grammar 4.10 and 4.20
pStructure31 :: Spelling -> Parser TibetanWord
pStructure31 spelling = C10.pConstraint10 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 32
-- On the basis of the Tibetan spelling grammar 4.11 and 4.20
pStructure32 :: Spelling -> Parser TibetanWord
pStructure32 spelling = C11.pConstraint11 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 33
-- On the basis of the Tibetan spelling grammar 4.12 and 4.20
pStructure33 :: Spelling -> Parser TibetanWord
pStructure33 spelling = C12.pConstraint12 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 34
-- On the basis of the Tibetan spelling grammar 4.13 and 4.20
pStructure34 :: Spelling -> Parser TibetanWord
pStructure34 spelling = C13.pConstraint13 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 35
-- On the basis of the Tibetan spelling grammar 4.14 and 4.20
pStructure35 :: Spelling -> Parser TibetanWord
pStructure35 spelling = C14.pConstraint14 spelling <> C20.pConstraint20 spelling

-- Tibetan spelling structure 36
-- On the basis of the Tibetan spelling grammar 4.17 and 4.20
pStructure36 :: Spelling -> Parser TibetanWord
pStructure36 spelling =
    (MP.choice
        [ MP.try (C17.pConstraint17Ra spelling)
        , MP.try (C17.pConstraint17Ya spelling)
        ])
        <> C20.pConstraint20 spelling

-- Tibetan spelling structure 37
-- On the basis of the Tibetan spelling grammar 4.18 and 4.20
pStructure37 :: Spelling -> Parser TibetanWord
pStructure37 spelling = C18.pConstraint18 spelling <> C20.pConstraint20 spelling