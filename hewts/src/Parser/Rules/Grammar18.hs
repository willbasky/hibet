{-
Tibetan spelling grammar 4.18
-}

module Parser.Rules.Grammar18
    ( pGrammar18
    ) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import qualified Data.Text as T
import Data.Text (Text, pattern (:<), pattern (:>))
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.18

pGrammar18 :: Parser Text
pGrammar18 = do
    root <- pRoot1
    subRoot <- pRoot2
    vowel <- optional pVowel
    let consT = T.empty :> root :> subRoot
    pure $ maybe consT (consT :>) vowel

--
-- A root ཧ
pRoot1 :: Parser Char
pRoot1 = char (fetchChar consonants 29) <?> "A root ཧ"

-- ཕ
pRoot2 :: Parser Char
pRoot2 = char (fetchChar subConsonants 14) <?> "A subRoot ྥ"