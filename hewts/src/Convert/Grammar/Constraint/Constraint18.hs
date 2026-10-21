{-
Tibetan spelling grammar 4.18 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint18
    ( pConstraint18
    ) where

import Convert.Grammar.Parser
    ( SpellParser
    , isJoinedAt
    , markRoot
    , markVowel
    , vowelSlot
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token (Consonant (..), SubConsonant (..))
import qualified Text.Megaparsec as MP

-- | ཧྥ: a root ཧ with the subroot ཕ. Wylie writes it in full letters ("hpha"),
-- so the subroot is the plain letter ཕ there; the sign decides which it is.
pConstraint18 :: SpellParser TibetanSyllable
pConstraint18 = do
    root <- markRoot (MP.satisfy (GP.isSpecificConsonant Ch) MP.<?> "A root ཧ")
    subRoot <-
        markRoot (MP.satisfy (isJoinedAt SCph Cph) MP.<?> "A subroot ཕ")
    let stack = root <> subRoot
    vowel <- vowelSlot stack [markVowel GP.pVowel]
    pure (stack <> vowel)
