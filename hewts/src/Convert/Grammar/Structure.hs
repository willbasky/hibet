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

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
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
pStructure1 :: Parser TibetanWord
pStructure1 = MP.choice
    [ MP.try C01.pConstraint01WithLong
    , MP.try C01.pConstraint01Sanskrit
    ]

-- Tibetan spelling structure 2
-- On the basis of the Tibetan spelling grammar 4.8
pStructure2 :: Parser TibetanWord
pStructure2 = C08.pConstraint08

-- Tibetan spelling structure 3
-- On the basis of the Tibetan spelling grammar 4.9
pStructure3 :: Parser TibetanWord
pStructure3 = C09.pConstraint09

-- Tibetan spelling structure 4
-- On the basis of the Tibetan spelling grammar 4.10
pStructure4 :: Parser TibetanWord
pStructure4 = C10.pConstraint10

-- Tibetan spelling structure 5
-- On the basis of the Tibetan spelling grammar 4.11
pStructure5 :: Parser TibetanWord
pStructure5 = C11.pConstraint11

-- Tibetan spelling structure 6
-- On the basis of the Tibetan spelling grammar 4.12
pStructure6 :: Parser TibetanWord
pStructure6 = C12.pConstraint12

-- Tibetan spelling structure 7
-- On the basis of the Tibetan spelling grammar 4.13
pStructure7 :: Parser TibetanWord
pStructure7 = C13.pConstraint13

-- Tibetan spelling structure 8
-- On the basis of the Tibetan spelling grammar 4.14
pStructure8 :: Parser TibetanWord
pStructure8 = C14.pConstraint14

-- Tibetan spelling structure 9
-- On the basis of the Tibetan spelling grammar 4.14 and 4.15
pStructure9 :: Parser TibetanWord
pStructure9 = C14.pConstraint14 <> C15.pConstraint15

-- Tibetan spelling structure 10
-- On the basis of the Tibetan spelling grammar 4.11 and 4.15
pStructure10 :: Parser TibetanWord
pStructure10 = C11.pConstraint11 <> C15.pConstraint15

-- Tibetan spelling structure 11
-- On the basis of the Tibetan spelling grammar 4.12 and 4.15
pStructure11 :: Parser TibetanWord
pStructure11 = C12.pConstraint12 <> C15.pConstraint15

-- Tibetan spelling structure 12
-- On the basis of the Tibetan spelling grammar 4.13 and 4.15
pStructure12 :: Parser TibetanWord
pStructure12 = C13.pConstraint13 <> C15.pConstraint15

-- Tibetan spelling structure 13
-- On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
pStructure13 :: Parser TibetanWord
pStructure13 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C14.pConstraint14 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C14.pConstraint14 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 14
-- On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
pStructure14 :: Parser TibetanWord
pStructure14 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C11.pConstraint11 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C11.pConstraint11 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 15
-- On the basis of the Tibetan spelling grammar 4.12, 4.15, 4.16
pStructure15 :: Parser TibetanWord
pStructure15 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C12.pConstraint12 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C12.pConstraint12 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 16
-- On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
pStructure16 :: Parser TibetanWord
pStructure16 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C13.pConstraint13 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C13.pConstraint13 C16.pConstraint16Sa GP.pPostfixSa
        ]

parseSuffixPostfix :: Parser TibetanWord -> Parser TibetanWord -> Parser Token -> Parser TibetanWord
parseSuffixPostfix base suffix post = do
    b <- base
    s <- suffix
    p <- mark Postfix post
    pure (b <> s <> p)

-- Tibetan spelling structure 17
-- On the basis of the Tibetan spelling grammar 4.15
pStructure17 :: Parser TibetanWord
pStructure17 = do
    root <- mark Root GP.pRootConsonant
    vowel <- MP.optional (mark Vowel GP.pVowel)
    suffix <- C15.pConstraint15
    pure (root <> fromMaybe mempty vowel <> suffix)

-- Tibetan spelling structure 18
-- On the basis of the Tibetan spelling grammar 4.8 and 4.15
pStructure18 :: Parser TibetanWord
pStructure18 = C08.pConstraint08 <> C15.pConstraint15

-- Tibetan spelling structure 19
-- On the basis of the Tibetan spelling grammar 4.9 and 4.15
pStructure19 :: Parser TibetanWord
pStructure19 = C09.pConstraint09 <> C15.pConstraint15

-- Tibetan spelling structure 20
-- On the basis of the Tibetan spelling grammar 4.10 and 4.15
pStructure20 :: Parser TibetanWord
pStructure20 = C10.pConstraint10 <> C15.pConstraint15

-- Tibetan spelling structure 21
-- On the basis of the Tibetan spelling grammar 4.1, 4.14, 4.15
pStructure21 :: Parser TibetanWord
pStructure21 =
    MP.choice
        [ MP.try $ rootSuffixPostfix C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ rootSuffixPostfix C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 22
-- On the basis of the Tibetan spelling grammar 4.8, 4.14, 4.15
pStructure22 :: Parser TibetanWord
pStructure22 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C08.pConstraint08 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C08.pConstraint08 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 23
-- On the basis of the Tibetan spelling grammar 4.9, 4.14, 4.15
pStructure23 :: Parser TibetanWord
pStructure23 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C09.pConstraint09 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C09.pConstraint09 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 24
-- On the basis of the Tibetan spelling grammar 4.10, 4.14, 4.15
pStructure24 :: Parser TibetanWord
pStructure24 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C10.pConstraint10 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C10.pConstraint10 C16.pConstraint16Sa GP.pPostfixSa
        ]

rootSuffixPostfix :: Parser TibetanWord -> Parser Token -> Parser TibetanWord
rootSuffixPostfix suffix post = do
    root <- mark Root GP.pRootConsonant
    vowel <- MP.optional (mark Vowel GP.pVowel)
    s <- suffix
    p <- mark Postfix post
    pure (root <> fromMaybe mempty vowel <> s <> p)

-- Tibetan spelling structure 25
-- On the basis of the Tibetan spelling grammar 4.17
pStructure25 :: Parser TibetanWord
pStructure25 =
    MP.choice
        [ MP.try C17.pConstraint17Ra
        , MP.try C17.pConstraint17Ya
        ]

-- Tibetan spelling structure 26
-- On the basis of the Tibetan spelling grammar 4.18
pStructure26 :: Parser TibetanWord
pStructure26 = C18.pConstraint18

-- Tibetan spelling structure 27
-- On the basis of the Tibetan spelling grammar 4.19
pStructure27 :: Parser TibetanWord
pStructure27 = C19.pConstraint19

-- Tibetan spelling structure 28
-- On the basis of the Tibetan spelling grammar 4.1 and 4.20
pStructure28 :: Parser TibetanWord
pStructure28 = C01.pConstraint01 <> C20.pConstraint20

-- Tibetan spelling structure 29
-- On the basis of the Tibetan spelling grammar 4.8 and 4.20
pStructure29 :: Parser TibetanWord
pStructure29 = C08.pConstraint08 <> C20.pConstraint20

-- Tibetan spelling structure 30
-- On the basis of the Tibetan spelling grammar 4.9 and 4.20
pStructure30 :: Parser TibetanWord
pStructure30 = C09.pConstraint09 <> C20.pConstraint20

-- Tibetan spelling structure 31
-- On the basis of the Tibetan spelling grammar 4.10 and 4.20
pStructure31 :: Parser TibetanWord
pStructure31 = C10.pConstraint10 <> C20.pConstraint20

-- Tibetan spelling structure 32
-- On the basis of the Tibetan spelling grammar 4.11 and 4.20
pStructure32 :: Parser TibetanWord
pStructure32 = C11.pConstraint11 <> C20.pConstraint20

-- Tibetan spelling structure 33
-- On the basis of the Tibetan spelling grammar 4.12 and 4.20
pStructure33 :: Parser TibetanWord
pStructure33 = C12.pConstraint12 <> C20.pConstraint20

-- Tibetan spelling structure 34
-- On the basis of the Tibetan spelling grammar 4.13 and 4.20
pStructure34 :: Parser TibetanWord
pStructure34 = C13.pConstraint13 <> C20.pConstraint20

-- Tibetan spelling structure 35
-- On the basis of the Tibetan spelling grammar 4.14 and 4.20
pStructure35 :: Parser TibetanWord
pStructure35 = C14.pConstraint14 <> C20.pConstraint20

-- Tibetan spelling structure 36
-- On the basis of the Tibetan spelling grammar 4.17 and 4.20
pStructure36 :: Parser TibetanWord
pStructure36 =
    (MP.choice
        [ MP.try C17.pConstraint17Ra
        , MP.try C17.pConstraint17Ya
        ])
        <> C20.pConstraint20

-- Tibetan spelling structure 37
-- On the basis of the Tibetan spelling grammar 4.18 and 4.20
pStructure37 :: Parser TibetanWord
pStructure37 = C18.pConstraint18 <> C20.pConstraint20