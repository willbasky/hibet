{-
Tibetan spelling grammar 4.13
-}

module Parser.Rules.Grammar13 (pGrammar13) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.13
pGrammar13 :: Parser Text
pGrammar13 =
    choice
        [ try $ parse13 pPrefixBa pSuperfixSa (pSubfixYa <|> pSubfixRa)
        , try $ parse13 pPrefixBa pSuperfixRa pSubfixYa
        ]

parse13 :: Parser Char -> Parser Char -> Parser Char -> Parser Text
parse13 parsePrefix parseSuperfix parseSubfix = do
    prefix <- parsePrefix
    superfix <- parseSuperfix
    root <- pRoot
    subfix <- parseSubfix
    vowel <- optional pVowel
    let consT = T.empty :> prefix :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- root group [ 'ཀ', 'ག' ]
roots :: HashSet Char
roots = fetchChars subConsonants [1, 3]

pRoot :: Parser Char
pRoot =
    satisfy (`member` roots)
        <?> "A root from [ 'ཀ', 'ག' ]"