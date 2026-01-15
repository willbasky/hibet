{-
Tibetan spelling structure 8
On the basis of the Tibetan spelling grammar 4.14
-}

module Parser.Structure.Structure8 (structure8) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

structure8 :: Parser Text
structure8 =
    choice
        [ parse_8_1
        , parse_8_2
        , parse_8_3
        , parse_8_4
        , parse_8_5
        ]

--
-- (1) root group [ 'ཅ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཞ', 'ཟ', 'ཡ', 'ཤ', 'ས' ] with prefix ག.
roots1 :: HashSet Char
roots1 = fetchChars consonants [ 5, 8, 9, 11, 12, 17, 21, 22, 24, 27, 28]

pRoot1 :: Parser Char
pRoot1 =
    satisfy (`member` roots1)
        <?> "A root from [ 'ཅ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཞ', 'ཟ', 'ཡ', 'ཤ', 'ས' ]"

parse_8_1 :: Parser Text
parse_8_1 = do
    prefix <- pPrefixGa
    root <- pRoot1
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (2) root group [ 'ཀ', 'ག', 'ང', 'པ', 'བ', 'མ' ] with prefix ད.
roots2 :: HashSet Char
roots2 = fetchChars consonants [ 1, 3, 4, 13, 15, 16 ]

pRoot2 :: Parser Char
pRoot2 =
    satisfy (`member` roots2)
        <?> "A root from [ 'ཀ', 'ག', 'ང', 'པ', 'བ', 'མ' ]"

parse_8_2 :: Parser Text
parse_8_2 = do
    prefix <- pPrefixDa
    root <- pRoot2
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (3) root group [ 'ཀ', 'ག', 'ཅ', 'ཏ', 'ད', 'ཙ', 'ཞ', 'ཟ', 'ཤ', 'ས' ] with prefix བ.
roots3 :: HashSet Char
roots3 = fetchChars consonants [ 1, 3, 5, 9, 11, 17, 21, 22, 27, 28 ]

pRoot3 :: Parser Char
pRoot3 =
    satisfy (`member` roots3)
        <?> "A root from [ 'ཀ', 'ག', 'ཅ', 'ཏ', 'ད', 'ཙ', 'ཞ', 'ཟ', 'ཤ', 'ས' ]"

parse_8_3 :: Parser Text
parse_8_3 = do
    prefix <- pPrefixBa
    root <- pRoot3
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (4) root group [ 'ཁ', 'ག', 'ང', 'ཆ', 'ཇ', 'ཉ', 'ཐ', 'ད', 'ན', 'ཚ', 'ཛ' ] with prefix མ.
roots4 :: HashSet Char
roots4 = fetchChars consonants [ 2, 3, 4, 6, 7, 8, 10, 11, 12, 18, 19 ]

pRoot4 :: Parser Char
pRoot4 =
    satisfy (`member` roots4)
        <?> "A root from [ 'ཁ', 'ག', 'ང', 'ཆ', 'ཇ', 'ཉ', 'ཐ', 'ད', 'ན', 'ཚ', 'ཛ' ]"

parse_8_4 :: Parser Text
parse_8_4 = do
    prefix <- pPrefixMa
    root <- pRoot4
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (5) root group [ 'ཁ', 'ག', 'ཆ', 'ཇ', 'ཐ', 'ད', 'ཕ', 'བ', 'ཚ', 'ཛ' ] with prefix འ.
roots5 :: HashSet Char
roots5 = fetchChars consonants [  2, 3, 6, 7, 10, 11, 14, 15, 18, 19 ]

pRoot5 :: Parser Char
pRoot5 =
    satisfy (`member` roots5)
        <?> "A root from [ 'ཁ', 'ག', 'ཆ', 'ཇ', 'ཐ', 'ད', 'ཕ', 'བ', 'ཚ', 'ཛ' ]"

parse_8_5 :: Parser Text
parse_8_5 = do
    prefix <- pPrefixA
    root <- pRoot5
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root
    pure $ maybe consT (consT :>) vowel