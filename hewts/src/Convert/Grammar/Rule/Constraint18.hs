{-
Tibetan spelling grammar 4.18 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint18
    ( pConstraint18
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Token
  ( Consonant (..)
  , SubConsonant (..)
  , Token
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))
import Data.Maybe (maybeToList)

pConstraint18 :: Parser [Token]
pConstraint18 = do
    root <- MP.satisfy (GP.isSpecificConsonant Ch) <?> "A root ཧ"
    subRoot <- MP.satisfy (GP.isSpecificSubConsonant SCph) <?> "A subRoot ཕ"
    vowel <- MP.optional GP.pVowel
    pure (root : subRoot : maybeToList vowel)