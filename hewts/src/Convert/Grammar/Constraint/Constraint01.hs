{- Tibetan spelling grammar 4.1 (token parser variant) -}

module Convert.Grammar.Constraint.Constraint01
    ( pConstraint01
    , pConstraint01WithLong
    , pConstraint01Sanskrit
    ) where

import Convert.Grammar.Parser (SpellParser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, markS)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (choice, optional)
import qualified Text.Megaparsec as MP

pConstraint01 :: Spelling -> SpellParser TibetanSyllable
pConstraint01 = \case
    Tibetan -> do
        root <- markS Root GP.pRootConsonant
        vowel <- optional (markS Vowel GP.pVowel)
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- markS Root GP.pRootConsonant
        vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
        pure (root <> vowel)

pConstraint01WithLong :: Spelling -> SpellParser TibetanSyllable
pConstraint01WithLong = \case
    Tibetan -> do
        root <- markS Root GP.pRootConsonant
        vowel <-
            optional
                (choice [markS Vowel GP.pVowel, markS Vowel GP.pVowelLongA])
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- markS Root GP.pRootConsonant
        vowel <-
            MP.choice
                [ markS Vowel GP.pVowel
                , markS Vowel GP.pVowelLongA
                , markS ImplicitVowel GP.pImplicitA
                ]
        pure (root <> vowel)

pConstraint01Sanskrit :: Spelling -> SpellParser TibetanSyllable
pConstraint01Sanskrit = \case
    Tibetan -> do
        root <- markS Root GP.pSanskrit
        vowel <- optional (markS Vowel GP.pVowel)
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- markS Root GP.pSanskrit
        vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
        pure (root <> vowel)

-- The Wylie arm: the vowel slot is the only difference - every Wylie syllable
-- writes its vowel, either as a vowel letter or as the letter @a@ that never
-- prints, so it is obligatory.
