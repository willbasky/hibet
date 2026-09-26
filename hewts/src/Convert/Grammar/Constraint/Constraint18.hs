{-
Tibetan spelling grammar 4.18 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint18
    ( pConstraint18
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

pConstraint18 :: Spelling -> Parser TibetanWord
pConstraint18 = \case
    Tibetan -> do
        root <- mark Root (MP.satisfy (GP.isSpecificConsonant Ch) MP.<?> "A root ཧ")
        subRoot <-
            mark Root (MP.satisfy (GP.isSpecificSubConsonant SCph) MP.<?> "A subroot ཕ")
        vowel <- MP.optional (mark Vowel GP.pVowel)
        pure (root <> subRoot <> fromMaybe mempty vowel)
    Wylie -> do
        root <- mark Root (MP.satisfy (GP.isSpecificConsonant Ch) MP.<?> "A root ཧ")
        subRoot <-
            mark Root (MP.satisfy (GP.isSpecificConsonant Cph) MP.<?> "A subroot ཕ")
        vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
        pure (root <> subRoot <> vowel)

-- The Wylie arm: ཧྥ is written in full letters ("hpha"), so the subroot is the
-- plain letter ཕ and the vowel is always written.
