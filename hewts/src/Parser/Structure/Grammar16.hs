{-
Tibetan spelling grammar 4.16
-}

module Parser.Structure.Grammar16
    ( pGrammar16Da
    , pGrammar16Sa
    ) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.16

--
-- Suffix group [ 'ན', 'ར', 'ལ' ] before postfix ད.
suffixPostfixDa :: HashSet Char
suffixPostfixDa = fetchChars consonants [12, 25, 26]

pGrammar16Da :: Parser Char
pGrammar16Da =
    satisfy (`member` suffixPostfixDa)
        <?> "A suffix from [ 'ན', 'ར', 'ལ' ]"

-- Suffix group [ 'ག', 'ང', 'བ', 'མ' ] before postfix ས.
suffixPostfixSa :: HashSet Char
suffixPostfixSa = fetchChars consonants [3, 4, 15, 16]

pGrammar16Sa :: Parser Char
pGrammar16Sa =
    satisfy (`member` suffixPostfixSa)
        <?> "A suffix from [ 'ག', 'ང', 'བ', 'མ' ]"
