{-
Token-level Tibetan spelling structures 1-8.

Translation of Convert.Grammar.Structures (char level) to the token level.
Structures match syllable tokens only; punctuation is left unconsumed so
it survives and is handled separately by a future top-level dispatcher.
-}

module Convert.Grammar.Rule
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
import qualified Convert.Grammar.Rule.Constraint01 as C01
import qualified Convert.Grammar.Rule.Constraint08 as C08
import qualified Convert.Grammar.Rule.Constraint09 as C09
import qualified Convert.Grammar.Rule.Constraint10 as C10
import qualified Convert.Grammar.Rule.Constraint11 as C11
import qualified Convert.Grammar.Rule.Constraint12 as C12
import qualified Convert.Grammar.Rule.Constraint13 as C13
import qualified Convert.Grammar.Rule.Constraint14 as C14
import qualified Convert.Grammar.Rule.Constraint15 as C15
import qualified Convert.Grammar.Rule.Constraint16 as C16
import qualified Convert.Grammar.Rule.Constraint17 as C17
import qualified Convert.Grammar.Rule.Constraint18 as C18
import qualified Convert.Grammar.Rule.Constraint19 as C19
import qualified Convert.Grammar.Rule.Constraint20 as C20
import Convert.Token (Token)
import Data.Maybe (maybeToList)
import qualified Text.Megaparsec as MP

-- Tibetan spelling structure 1
-- On the basis of the Tibetan spelling grammar 4.1
pStructure1 :: Parser [Token]
pStructure1 = MP.choice
    [ MP.try C01.pConstraint01WithLong
    , MP.try C01.pConstraint01Sanskrit
    ]

-- Tibetan spelling structure 2
-- On the basis of the Tibetan spelling grammar 4.8
pStructure2 :: Parser [Token]
pStructure2 = C08.pConstraint08

-- Tibetan spelling structure 3
-- On the basis of the Tibetan spelling grammar 4.9
pStructure3 :: Parser [Token]
pStructure3 = C09.pConstraint09

-- Tibetan spelling structure 4
-- On the basis of the Tibetan spelling grammar 4.10
pStructure4 :: Parser [Token]
pStructure4 = C10.pConstraint10

-- Tibetan spelling structure 5
-- On the basis of the Tibetan spelling grammar 4.11
pStructure5 :: Parser [Token]
pStructure5 = C11.pConstraint11

-- Tibetan spelling structure 6
-- On the basis of the Tibetan spelling grammar 4.12
pStructure6 :: Parser [Token]
pStructure6 = C12.pConstraint12

-- Tibetan spelling structure 7
-- On the basis of the Tibetan spelling grammar 4.13
pStructure7 :: Parser [Token]
pStructure7 = C13.pConstraint13

-- Tibetan spelling structure 8
-- On the basis of the Tibetan spelling grammar 4.14
pStructure8 :: Parser [Token]
pStructure8 = C14.pConstraint14

-- Tibetan spelling structure 9
-- On the basis of the Tibetan spelling grammar 4.14 and 4.15
pStructure9 :: Parser [Token]
pStructure9 = C14.pConstraint14 <> C15.pConstraint15

-- Tibetan spelling structure 10
-- On the basis of the Tibetan spelling grammar 4.11 and 4.15
pStructure10 :: Parser [Token]
pStructure10 = C11.pConstraint11 <> C15.pConstraint15

-- Tibetan spelling structure 11
-- On the basis of the Tibetan spelling grammar 4.12 and 4.15
pStructure11 :: Parser [Token]
pStructure11 = C12.pConstraint12 <> C15.pConstraint15

-- Tibetan spelling structure 12
-- On the basis of the Tibetan spelling grammar 4.13 and 4.15
pStructure12 :: Parser [Token]
pStructure12 = C13.pConstraint13 <> C15.pConstraint15

-- Tibetan spelling structure 13
-- On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
pStructure13 :: Parser [Token]
pStructure13 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C14.pConstraint14 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C14.pConstraint14 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 14
-- On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
pStructure14 :: Parser [Token]
pStructure14 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C11.pConstraint11 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C11.pConstraint11 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 15
-- On the basis of the Tibetan spelling grammar 4.12, 4.15, 4.16
pStructure15 :: Parser [Token]
pStructure15 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C12.pConstraint12 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C12.pConstraint12 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 16
-- On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
pStructure16 :: Parser [Token]
pStructure16 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C13.pConstraint13 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C13.pConstraint13 C16.pConstraint16Sa GP.pPostfixSa
        ]

parseSuffixPostfix :: Parser [Token] -> Parser [Token] -> Parser Token -> Parser [Token]
parseSuffixPostfix base suffix post = do
    b <- base
    s <- suffix
    p <- post
    pure (b <> s <> [p])

-- Tibetan spelling structure 17
-- On the basis of the Tibetan spelling grammar 4.15
pStructure17 :: Parser [Token]
pStructure17 = do
    root <- GP.pRootConsonant
    vowel <- MP.optional GP.pVowel
    suffix <- C15.pConstraint15
    pure (root : maybeToList vowel <> suffix)

-- Tibetan spelling structure 18
-- On the basis of the Tibetan spelling grammar 4.8 and 4.15
pStructure18 :: Parser [Token]
pStructure18 = C08.pConstraint08 <> C15.pConstraint15

-- Tibetan spelling structure 19
-- On the basis of the Tibetan spelling grammar 4.9 and 4.15
pStructure19 :: Parser [Token]
pStructure19 = C09.pConstraint09 <> C15.pConstraint15

-- Tibetan spelling structure 20
-- On the basis of the Tibetan spelling grammar 4.10 and 4.15
pStructure20 :: Parser [Token]
pStructure20 = C10.pConstraint10 <> C15.pConstraint15

-- Tibetan spelling structure 21
-- On the basis of the Tibetan spelling grammar 4.1, 4.14, 4.15
pStructure21 :: Parser [Token]
pStructure21 =
    MP.choice
        [ MP.try $ rootSuffixPostfix C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ rootSuffixPostfix C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 22
-- On the basis of the Tibetan spelling grammar 4.8, 4.14, 4.15
pStructure22 :: Parser [Token]
pStructure22 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C08.pConstraint08 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C08.pConstraint08 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 23
-- On the basis of the Tibetan spelling grammar 4.9, 4.14, 4.15
pStructure23 :: Parser [Token]
pStructure23 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C09.pConstraint09 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C09.pConstraint09 C16.pConstraint16Sa GP.pPostfixSa
        ]

-- Tibetan spelling structure 24
-- On the basis of the Tibetan spelling grammar 4.10, 4.14, 4.15
pStructure24 :: Parser [Token]
pStructure24 =
    MP.choice
        [ MP.try $ parseSuffixPostfix C10.pConstraint10 C16.pConstraint16Da GP.pPostfixDa
        , MP.try $ parseSuffixPostfix C10.pConstraint10 C16.pConstraint16Sa GP.pPostfixSa
        ]

rootSuffixPostfix :: Parser [Token] -> Parser Token -> Parser [Token]
rootSuffixPostfix suffix post = do
    root <- GP.pRootConsonant
    vowel <- MP.optional GP.pVowel
    s <- suffix
    p <- post
    pure (root : maybeToList vowel <> s <> [p])

-- Tibetan spelling structure 25
-- On the basis of the Tibetan spelling grammar 4.17
pStructure25 :: Parser [Token]
pStructure25 =
    MP.choice
        [ MP.try C17.pConstraint17Ra
        , MP.try C17.pConstraint17Ya
        ]

-- Tibetan spelling structure 26
-- On the basis of the Tibetan spelling grammar 4.18
pStructure26 :: Parser [Token]
pStructure26 = C18.pConstraint18

-- Tibetan spelling structure 27
-- On the basis of the Tibetan spelling grammar 4.19
pStructure27 :: Parser [Token]
pStructure27 = C19.pConstraint19

-- Tibetan spelling structure 28
-- On the basis of the Tibetan spelling grammar 4.1 and 4.20
pStructure28 :: Parser [Token]
pStructure28 = C01.pConstraint01 <> C20.pConstraint20

-- Tibetan spelling structure 29
-- On the basis of the Tibetan spelling grammar 4.8 and 4.20
pStructure29 :: Parser [Token]
pStructure29 = C08.pConstraint08 <> C20.pConstraint20

-- Tibetan spelling structure 30
-- On the basis of the Tibetan spelling grammar 4.9 and 4.20
pStructure30 :: Parser [Token]
pStructure30 = C09.pConstraint09 <> C20.pConstraint20

-- Tibetan spelling structure 31
-- On the basis of the Tibetan spelling grammar 4.10 and 4.20
pStructure31 :: Parser [Token]
pStructure31 = C10.pConstraint10 <> C20.pConstraint20

-- Tibetan spelling structure 32
-- On the basis of the Tibetan spelling grammar 4.11 and 4.20
pStructure32 :: Parser [Token]
pStructure32 = C11.pConstraint11 <> C20.pConstraint20

-- Tibetan spelling structure 33
-- On the basis of the Tibetan spelling grammar 4.12 and 4.20
pStructure33 :: Parser [Token]
pStructure33 = C12.pConstraint12 <> C20.pConstraint20

-- Tibetan spelling structure 34
-- On the basis of the Tibetan spelling grammar 4.13 and 4.20
pStructure34 :: Parser [Token]
pStructure34 = C13.pConstraint13 <> C20.pConstraint20

-- Tibetan spelling structure 35
-- On the basis of the Tibetan spelling grammar 4.14 and 4.20
pStructure35 :: Parser [Token]
pStructure35 = C14.pConstraint14 <> C20.pConstraint20

-- Tibetan spelling structure 36
-- On the basis of the Tibetan spelling grammar 4.17 and 4.20
pStructure36 :: Parser [Token]
pStructure36 =
    (MP.choice
        [ MP.try C17.pConstraint17Ra
        , MP.try C17.pConstraint17Ya
        ])
        <> C20.pConstraint20

-- Tibetan spelling structure 37
-- On the basis of the Tibetan spelling grammar 4.18 and 4.20
pStructure37 :: Parser [Token]
pStructure37 = C18.pConstraint18 <> C20.pConstraint20