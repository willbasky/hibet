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
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Rule.Constraint01 as C01
import qualified Convert.Grammar.Rule.Constraint08 as C08
import qualified Convert.Grammar.Rule.Constraint09 as C09
import qualified Convert.Grammar.Rule.Constraint10 as C10
import qualified Convert.Grammar.Rule.Constraint11 as C11
import qualified Convert.Grammar.Rule.Constraint12 as C12
import qualified Convert.Grammar.Rule.Constraint13 as C13
import qualified Convert.Grammar.Rule.Constraint14 as C14
import Convert.Token (Token)
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