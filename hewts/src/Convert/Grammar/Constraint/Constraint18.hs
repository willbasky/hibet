{-
Tibetan spelling grammar 4.18 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint18
    ( pConstraint18
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark, vowelSlot)
import Data.Maybe (fromMaybe)
import Convert.Token
  ( Consonant (..)
  , SubConsonant (..)
  , Token
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))

pConstraint18 :: Spelling -> Parser TibetanWord
pConstraint18 spelling = do
    root <- mark Root (MP.satisfy (GP.isSpecificConsonant Ch) MP.<?> "A root ཧ")
    subRoot <- mark Root (MP.satisfy (GP.isSpecificSubConsonant SCph) MP.<?> "A subRoot ཕ")
    vowel <- MP.optional (vowelSlot spelling)
    pure (root <> subRoot <> fromMaybe mempty vowel)