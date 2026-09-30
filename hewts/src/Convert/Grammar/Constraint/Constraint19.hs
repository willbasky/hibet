{-
Tibetan spelling grammar 4.19 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint19
    ( pConstraint19
    ) where

import qualified Convert.Grammar.Constraint.Constraint15 as C15
import qualified Convert.Grammar.Constraint.Constraint18 as C18
import Convert.Grammar.Parser (SpellParser, Spelling (..))
import Convert.Grammar.Syllable (TibetanSyllable)

pConstraint19 :: Spelling -> SpellParser TibetanSyllable
pConstraint19 spelling = do
    struct <- C18.pConstraint18 spelling
    suffix <- C15.pConstraint15 spelling
    pure (struct <> suffix)
