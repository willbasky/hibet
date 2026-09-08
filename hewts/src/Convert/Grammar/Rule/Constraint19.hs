{-
Tibetan spelling grammar 4.19 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint19
    ( pConstraint19
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Rule.Constraint15 as C15
import qualified Convert.Grammar.Rule.Constraint18 as C18
import Convert.Token (Token)

pConstraint19 :: Parser [Token]
pConstraint19 = do
    struct <- C18.pConstraint18
    suffix <- C15.pConstraint15
    pure (struct <> suffix)