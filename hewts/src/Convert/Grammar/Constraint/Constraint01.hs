{-
Tibetan spelling grammar 4.1 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint01 (pConstraint01, pConstraint01WithLong, pConstraint01Sanskrit) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token (Token)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (choice, optional)

pConstraint01 :: Parser TibetanWord
pConstraint01 = do
    root <- mark Root GP.pRootConsonant
    vowel <- optional (mark Vowel GP.pVowel)
    pure (root <> fromMaybe [] vowel)

pConstraint01WithLong :: Parser TibetanWord
pConstraint01WithLong = do
    root <- mark Root GP.pRootConsonant
    vowel <- optional $ choice [mark Vowel GP.pVowel, mark Vowel GP.pVowelLongA]
    pure (root <> fromMaybe [] vowel)

pConstraint01Sanskrit :: Parser TibetanWord
pConstraint01Sanskrit = do
    root <- mark Root GP.pSanskrit
    vowel <- optional (mark Vowel GP.pVowel)
    pure (root <> fromMaybe [] vowel)