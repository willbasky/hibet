{-
Tibetan spelling grammar 4.19 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint19
    ( pConstraint19
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Constraint.Constraint15 as C15
import qualified Convert.Grammar.Constraint.Constraint18 as C18
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token (Token)

pConstraint19 :: Spelling -> Parser TibetanWord
pConstraint19 spelling = do
    struct <- C18.pConstraint18 spelling
    suffix <- C15.pConstraint15 spelling
    pure (struct <> suffix)