{-
Tibetan spelling grammar 4.20 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint20
    ( pConstraint20
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
import Data.Maybe (fromMaybe)

pConstraint20 :: Parser TibetanWord
pConstraint20 = do
    root <- mark Root (MP.satisfy (GP.isSpecificConsonant C') MP.<?> "A root འ")
    -- འ takes either a vowel, or a second root (Def 4.10's special case:
    -- "a consonant alphabet with a consonant alphabet"), so ང and མ are roots here
    vowelA <- MP.optional $
        MP.choice
            [ mark Vowel GP.pVowel
            , mark Root (MP.satisfy (GP.isSpecificSubConsonant SCng) MP.<?> "A subConsonant ང")
            , mark Root (MP.satisfy (GP.isSpecificSubConsonant SCm) MP.<?> "A subConsonant མ")
            ]
    pure (root <> fromMaybe [] vowelA)