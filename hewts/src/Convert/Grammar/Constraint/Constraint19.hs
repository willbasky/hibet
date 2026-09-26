{-
Tibetan spelling grammar 4.19 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint19
    ( pConstraint19
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Constraint.Constraint15 as C15
import qualified Convert.Grammar.Constraint.Constraint18 as C18
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token (Token)

pConstraint19 :: Parser TibetanWord
pConstraint19 = do
    struct <- C18.pConstraint18
    suffix <- C15.pConstraint15
    pure (struct <> suffix)