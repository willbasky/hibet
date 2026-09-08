{-
Tibetan spelling grammar 4.20 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint20
    ( pConstraint20
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Token
  ( Consonant (..)
  , SubConsonant (..)
  , Token
  )
import qualified Text.Megaparsec as MP

pConstraint20 :: Parser [Token]
pConstraint20 = do
    root <- MP.satisfy (GP.isSpecificConsonant C') MP.<?> "A root འ"
    vowelA <- MP.optional $ MP.choice
        [ GP.pVowel
        , MP.satisfy (GP.isSpecificSubConsonant SCng) MP.<?> "A subConsonant ང"
        , MP.satisfy (GP.isSpecificSubConsonant SCm) MP.<?> "A subConsonant མ"
        ]
    pure (root : maybe [] pure vowelA)