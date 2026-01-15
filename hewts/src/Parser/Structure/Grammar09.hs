{-
Tibetan spelling grammar 4.9
-}

module Parser.Structure.Grammar09 (pGrammar9) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.9

pGrammar9 :: Parser Text 
pGrammar9 =  
    choice
        [ try $ parseGrammar9 pRootSubfixWa pSubfixWa
        , try $ parseGrammar9 pRootSubfixYa pSubfixYa
        , try $ parseGrammar9 pRootSubfixRa pSubfixRa
        , try $ parseGrammar9 pRootSubfixLa pSubfixLa
        ]

parseGrammar9 :: Parser Char -> Parser Char -> Parser Text
parseGrammar9 parseRoot parseSubfix  = do
    root <- parseRoot
    subfix <- parseSubfix
    vowel <- optional pVowel
    let consT = T.empty :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- Roots above subfix 'ཝ' are [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]
rootWaSubfix :: HashSet Char
rootWaSubfix = fetchChars consonants [1, 2, 3, 8, 11, 18, 21, 22, 25, 26, 27, 29]

pRootSubfixWa :: Parser Char
pRootSubfixWa =
    satisfy (`member` rootWaSubfix)
        <?> "Subfix ཝ should be placed below the root [ 'ཀ', 'ཁ', 'ག', 'ཉ', 'ད', 'ཚ', 'ཞ', 'ཟ', 'ར', 'ལ', 'ཤ', 'ཧ' ]"

--
-- Roots above subfix 'ཡ' are [ 'ཀ', 'ཁ', 'ག', 'པ', 'ཕ', 'བ', 'མ' ]
rootSubfixYa :: HashSet Char
rootSubfixYa = fetchChars consonants [1, 2, 3, 13, 14, 15, 16]

pRootSubfixYa :: Parser Char
pRootSubfixYa =
    satisfy (`member` rootSubfixYa)
        <?> "Subfix ཡ should be placed below the root [ 'ཀ', 'ཁ', 'ག', 'པ', 'ཕ', 'བ', 'མ' ]"

--
-- Roots above subfix 'ར' are [ 'ཀ', 'ཁ', 'ག', 'ཏ', 'ཐ', 'ད', 'པ', 'ཕ', 'བ', 'མ', 'ས', 'ཧ' ]
rootSubfixRa :: HashSet Char
rootSubfixRa = fetchChars consonants [1, 2, 3, 9, 10, 11, 13, 14, 15, 16, 28, 29]

pRootSubfixRa :: Parser Char
pRootSubfixRa =
    satisfy (`member` rootSubfixRa)
        <?> "Subfix ར should be placed below the root [ 'ཀ', 'ཁ', 'ག', 'ཏ', 'ཐ', 'ད', 'པ', 'ཕ', 'བ', 'མ', 'ས', 'ཧ' ]"

-- Roots above subfix 'ལ' are [ 'ཀ', 'ག', 'བ', 'ཟ', 'ར', 'ས' ]
rootSubfixLa :: HashSet Char
rootSubfixLa = fetchChars consonants [1, 3, 15, 22, 25, 28]

pRootSubfixLa :: Parser Char
pRootSubfixLa =
    satisfy (`member` rootSubfixLa)
        <?> "Subfix ལ should be placed below the root [ 'ཀ', 'ག', 'བ', 'ཟ', 'ར', 'ས' ]"