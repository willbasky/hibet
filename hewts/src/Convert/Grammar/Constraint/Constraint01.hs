{- Tibetan spelling grammar 4.1 (token parser variant) -}

module Convert.Grammar.Constraint.Constraint01
    ( pConstraint01
    , pConstraint01WithLong
    , pConstraint01Sanskrit
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Text.Megaparsec (choice, optional)
import qualified Text.Megaparsec as MP

-- | The root a stack already spelled: the a-chen standing there is no
-- consonant waiting for its vowel - it already is that vowel. The Wylie
-- vowel slot may not take a second @a@ as this stack's then (@aa@ is two
-- syllables, as the reference reads it); the slot stays empty and the run
-- stays for the word parser, which reads the second letter as its own
-- stack.
rootIsA :: TibetanSyllable -> Bool
rootIsA = maybe False (GP.isImplicitA . snd) . Seq.lookup 0

pConstraint01 :: Spelling -> Parser TibetanSyllable
pConstraint01 = \case
    Tibetan -> do
        root <- mark Root GP.pRootConsonant
        vowel <- optional (mark Vowel GP.pVowel)
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- mark Root GP.pRootConsonant
        vowel <-
            MP.choice
                [ mark Vowel GP.pVowel
                , if rootIsA root then MP.empty else mark ImplicitVowel GP.pImplicitA
                ]
        pure (root <> vowel)

pConstraint01WithLong :: Spelling -> Parser TibetanSyllable
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
                , if rootIsA root then MP.empty else mark ImplicitVowel GP.pImplicitA
                ]
        pure (root <> vowel)

pConstraint01Sanskrit :: Spelling -> Parser TibetanSyllable
pConstraint01Sanskrit = \case
    Tibetan -> do
        root <- mark Root GP.pSanskrit
        vowel <- optional (mark Vowel GP.pVowel)
        pure (root <> fromMaybe mempty vowel)
    Wylie -> do
        root <- mark Root GP.pSanskrit
        vowel <-
            MP.choice
                [ mark Vowel GP.pVowel
                , if rootIsA root then MP.empty else mark ImplicitVowel GP.pImplicitA
                ]
        pure (root <> vowel)

-- The Wylie arm: the vowel slot is the only difference - every Wylie syllable
-- writes its vowel, either as a vowel letter or as the letter @a@ that never
-- prints, so it is obligatory.
