{-
Tibetan spelling grammar 4.1 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint01 (pConstraint01, pConstraint01WithLong, pConstraint01Sanskrit) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Token (Token)
import Data.Maybe (maybeToList)
import Text.Megaparsec (choice, optional)

pConstraint01 :: Parser [Token]
pConstraint01 = do
    root <- GP.pRootConsonant
    vowel <- optional GP.pVowel
    pure (root : maybeToList vowel)

pConstraint01WithLong :: Parser [Token]
pConstraint01WithLong = do
    root <- GP.pRootConsonant
    vowel <- optional $ choice [GP.pVowel, GP.pVowelLongA]
    pure (root : maybeToList vowel)

pConstraint01Sanskrit :: Parser [Token]
pConstraint01Sanskrit = do
    root <- GP.pSanskrit
    vowel <- optional GP.pVowel
    pure (root : maybeToList vowel)