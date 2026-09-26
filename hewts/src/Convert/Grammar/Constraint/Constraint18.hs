{-
Tibetan spelling grammar 4.18 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint18
    ( pConstraint18
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token
  ( Consonant (..)
  , SubConsonant (..)
  , Token
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))
import Data.Maybe (fromMaybe)

pConstraint18 :: Parser TibetanWord
pConstraint18 = do
    root <- mark Root (MP.satisfy (GP.isSpecificConsonant Ch) MP.<?> "A root ཧ")
    subRoot <- mark Root (MP.satisfy (GP.isSpecificSubConsonant SCph) MP.<?> "A subRoot ཕ")
    vowel <- MP.optional (mark Vowel GP.pVowel)
    pure (root <> subRoot <> fromMaybe [] vowel)