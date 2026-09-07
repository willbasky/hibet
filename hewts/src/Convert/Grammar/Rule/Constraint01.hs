{-
Tibetan spelling grammar 4.1 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint01 (pConstraint01, pConstraint01WithLong, pConstraint01Sanskrit) where

import Convert.Grammar.Parser
import Convert.Token (Token)
import Data.Maybe (maybeToList)
import Text.Megaparsec (choice, optional)

pConstraint01 :: Parser [Token]
pConstraint01 = do
    root <- pRootConsonant
    vowel <- optional pVowel
    pure (root : maybeToList vowel)

pConstraint01WithLong :: Parser [Token]
pConstraint01WithLong = do
    root <- pRootConsonant
    vowel <- optional $ choice [pVowel, pVowelLongA]
    pure (root : maybeToList vowel)

pConstraint01Sanskrit :: Parser [Token]
pConstraint01Sanskrit = do
    root <- pSanskrit
    vowel <- optional pVowel
    pure (root : maybeToList vowel)