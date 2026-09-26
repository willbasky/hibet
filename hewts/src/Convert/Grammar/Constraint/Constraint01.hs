{- Tibetan spelling grammar 4.1 (token parser variant) -}

module Convert.Grammar.Constraint.Constraint01
    ( pConstraint01
    , pConstraint01WithLong
    , pConstraint01Sanskrit
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token (Token)
import Text.Megaparsec (choice, optional)

pConstraint01 :: Spelling -> Parser TibetanWord
pConstraint01 spelling = do
    root <- mark Root GP.pRootConsonant
    vowel <- optional (vowelSlot spelling)
    pure (root <> fromMaybe mempty vowel)

pConstraint01WithLong :: Spelling -> Parser TibetanWord
pConstraint01WithLong spelling = do
    root <- mark Root GP.pRootConsonant
    vowel <- optional $ choice [vowelSlot spelling, mark Vowel GP.pVowelLongA]
    pure (root <> fromMaybe mempty vowel)

pConstraint01Sanskrit :: Spelling -> Parser TibetanWord
pConstraint01Sanskrit spelling = do
    root <- mark Root GP.pSanskrit
    vowel <- optional (vowelSlot spelling)
    pure (root <> fromMaybe mempty vowel)
