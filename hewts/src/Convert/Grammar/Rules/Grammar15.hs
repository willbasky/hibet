{-
Tibetan spelling grammar 4.15
-}

module Convert.Grammar.Rules.Grammar15 (pGrammar15) where

import Convert.Grammar.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

--
-- Suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ] .
suffixesGrammar15 :: HashSet Char
suffixesGrammar15 = fetchChars consonants [3, 4, 11, 12, 15, 16, 23, 25, 26, 28]

pGrammar15 :: Parser Char
pGrammar15 =
    satisfy (`member` suffixesGrammar15)
        <?> "A suffix from [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ]"