{-
Tibetan spelling grammar 4.10
-}

module Parser.Rules.Grammar10 (pGrammar10) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.10

pGrammar10 :: Parser Text 
pGrammar10 =  
    choice
        [ try $ parseGrammar10 pSuperfixRa pRoots1 pSubfixYa
        , try $ parseGrammar10 pSuperfixSa pRoots2 $ pSubfixYa <|> pSubfixRa
        , try $ parseGrammar10 pSuperfixSa pRoot3 pSubfixRa
        , try $ parseGrammar10 pSuperfixRa pRoot4 pSubfixWa 
        ]

parseGrammar10 :: Parser Char -> Parser Char -> Parser Char -> Parser Text
parseGrammar10 superfixParse parseRoot parseSubfix  = do
    superfix <- superfixParse
    root <- parseRoot
    subfix <- parseSubfix
    vowel <- optional pVowel
    let consT = T.empty :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (1) root group [ 'ཀ', 'ག', 'མ' ] under superfix ར and above subfix ཡ.
roots1 :: HashSet Char
roots1 = fetchChars subConsonants [1, 3, 16]

pRoots1 :: Parser Char
pRoots1 = satisfy (`member` roots1) <?> "A root from [ 'ཀ', 'ག', 'མ' ]"

--
-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] under superfix ས and above subfix ཡ or ར.
roots2 :: HashSet Char
roots2 = fetchChars subConsonants [1, 3, 13, 15, 16]

pRoots2 :: Parser Char
pRoots2 = satisfy (`member` roots2) <?> "A roots from [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ]"

-- ན
pRoot3 :: Parser Char
pRoot3 = char (fetchChar subConsonants 12) <?> "A root ན"

-- ཙ
pRoot4 :: Parser Char
pRoot4 = char (fetchChar subConsonants 17) <?> "A root ཙ"


