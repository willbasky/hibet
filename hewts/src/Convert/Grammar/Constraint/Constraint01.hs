{- Tibetan spelling grammar 4.1 (token parser variant) -}

module Convert.Grammar.Constraint.Constraint01
    ( pConstraint01
    , pConstraint01WithLong
    , pConstraint01Sanskrit
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (choice, optional)
import qualified Text.Megaparsec as MP

pConstraint01 :: Spelling -> Parser TibetanWord
pConstraint01 = \case
    Tibetan -> do
        root <- mark Root GP.pRootConsonant
        vowel <- optional (mark Vowel GP.pVowel)
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- mark Root GP.pRootConsonant
        vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
        pure (root <> vowel)

pConstraint01WithLong :: Spelling -> Parser TibetanWord
pConstraint01WithLong = \case
    Tibetan -> do
        root <- mark Root GP.pRootConsonant
        vowel <-
            optional
                (choice [mark Vowel GP.pVowel, mark Vowel GP.pVowelLongA])
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- mark Root GP.pRootConsonant
        vowel <-
            MP.choice
                [ mark Vowel GP.pVowel
                , mark Vowel GP.pVowelLongA
                , mark ImplicitVowel GP.pImplicitA
                ]
        pure (root <> vowel)

pConstraint01Sanskrit :: Spelling -> Parser TibetanWord
pConstraint01Sanskrit = \case
    Tibetan -> do
        root <- mark Root GP.pSanskrit
        vowel <- optional (mark Vowel GP.pVowel)
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- mark Root GP.pSanskrit
        vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
        pure (root <> vowel)

-- The Wylie arm: the vowel slot is the only difference - every Wylie syllable
-- writes its vowel, either as a vowel letter or as the letter @a@ that never
-- prints, so it is obligatory.
