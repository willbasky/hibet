{-
Tibetan spelling grammar 4.11
-}

module Parser.Rules.Grammar11 (pGrammar11) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pGrammar11 :: Parser Text
pGrammar11 =
        choice
            [ try $ parse11 pPrefixBa pSuperfixRa pRoot1
            , try $ parse11 pPrefixBa pSuperfixLa pRoot2
            , try $ parse11 pPrefixBa pSuperfixSa pRoot3
            ]

parse11 :: Parser Char -> Parser Char -> Parser Char -> Parser Text
parse11 parsePrefix parseSuperfix parseRoot = do
    prefix <- parsePrefix
    superfix <- parseSuperfix
    root <- parseRoot
    vowel <- optional pVowel
    let consT = T.empty :> prefix :> superfix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (1) root group [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཛ' ] with prefix བ under superfix ར.
roots1 :: HashSet Char
roots1 = fetchChars subConsonants [1, 3, 4, 7, 8, 9, 11, 12, 17, 19]

pRoot1 :: Parser Char
pRoot1 =
    satisfy (`member` roots1)
        <?> "A root from [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཛ' ]"

--
-- (2) root group [ 'ཏ', 'ད' ] with prefix བ under superfix ལ.
roots2 :: HashSet Char
roots2 = fetchChars subConsonants [9, 11]

pRoot2 :: Parser Char
pRoot2 = satisfy (`member` roots2) <?> "A root from [ 'ཏ', 'ད' ]"

--
-- (3) root group [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ' ] with prefix བ under superfix ས.
roots3 :: HashSet Char
roots3 = fetchChars subConsonants [1, 3, 4, 8, 9, 11, 12, 17]

pRoot3 :: Parser Char
pRoot3 =
    satisfy (`member` roots3)
        <?> "A root from [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ' ]"