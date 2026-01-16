{-
Tibetan spelling grammar 4.19
-}

module Parser.Rules.Grammar19
    ( pGrammar19
    ) where

import Parser.Common
import Parser.Rules.Grammar15 (pGrammar15)
import Parser.Rules.Grammar18 (pGrammar18)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import qualified Data.Text as T
import Data.Text (Text, pattern (:<), pattern (:>))
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.19

pGrammar19 :: Parser Text
pGrammar19 = do
    struct <- pGrammar18
    suffix <- pGrammar15
    pure $ struct :> suffix