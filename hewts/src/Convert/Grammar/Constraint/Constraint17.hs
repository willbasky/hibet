{-
Tibetan spelling grammar 4.17 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint17
    ( pConstraint17Ra
    , pConstraint17Ya
    ) where

import Convert.Grammar.Parser (Parser, SpellParser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, markS)
import Convert.Token
    ( Consonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

-- (1) root group [ 'ག', 'ད' ] above the subfix ར.
pConstraint17Ra :: Spelling -> SpellParser TibetanSyllable
pConstraint17Ra = \case
    Tibetan -> do
        root <- markS Root (pRootConsonant [Cg, Cd])
        ra <- markS Subfix GP.pSubfixRa
        wa <- markS Subfix GP.pSubfixWa
        vowel <- MP.optional (markS Vowel GP.pVowel)
        pure (root <> ra <> wa <> fromMaybe mempty vowel)
    Wylie -> do
        root <- markS Root (pRootConsonant [Cg, Cd])
        ra <- markS Subfix GP.pSubfixRaWylie
        wa <- markS Subfix GP.pSubfixWaWylie
        vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
        pure (root <> ra <> wa <> vowel)

-- (2) root ཕ above the subfix ཡ.
pConstraint17Ya :: Spelling -> SpellParser TibetanSyllable
pConstraint17Ya = \case
    Tibetan -> do
        root <- markS Root (pRootConsonant [Cph])
        ya <- markS Subfix GP.pSubfixYa
        wa <- markS Subfix GP.pSubfixWa
        vowel <- MP.optional (markS Vowel GP.pVowel)
        pure (root <> ya <> wa <> fromMaybe mempty vowel)
    Wylie -> do
        root <- markS Root (pRootConsonant [Cph])
        ya <- markS Subfix GP.pSubfixYaWylie
        wa <- markS Subfix GP.pSubfixWaWylie
        vowel <- MP.choice [markS Vowel GP.pVowel, markS ImplicitVowel GP.pImplicitA]
        pure (root <> ya <> wa <> vowel)

pRootConsonant :: [Consonant] -> Parser Token
pRootConsonant allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c | c `elem` allowed -> pure tok
        _ -> MP.empty

-- The Wylie arm: the root is a full letter either way; the subfixes are plain
-- letters and the vowel is always written, so each arm writs its letters in
-- full.
