{-
Tibetan spelling grammar 4.8
-}

module Parser.Rules.Grammar08 (pGrammar8) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.8

pGrammar8 :: Parser Text 
pGrammar8 =  
    choice
        [ try $ parseGrammar8 pSuperfixRa pRaSuperfixRoot
        , try $ parseGrammar8 pSuperfixLa pLaSuperfixRoot
        , try $ parseGrammar8 pSuperfixSa pSaSuperfixRoot
        ]

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pGrammar8 "སྨ"
-- Right "སྨ"

parseGrammar8 :: Parser Char -> Parser Char -> Parser Text
parseGrammar8 parseSuperfix parseRoot = do
    superfix <- parseSuperfix
    root <- parseRoot
    vowel <- optional pVowel
    let consT = T.empty :> superfix :> root
    pure $ maybe consT (consT :>) vowel

--
-- Roots under superfix 'ར' are [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]
raSuperfixRoot :: HashSet Char
raSuperfixRoot = fetchChars subConsonants [1, 3, 4, 7, 8, 9, 11, 12, 15, 16, 17, 19]

pRaSuperfixRoot :: Parser Char
pRaSuperfixRoot =
    satisfy (`member` raSuperfixRoot)
        <?> "Superfix ར should be placed above the root [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]"

--
-- Roots under superfix 'ལ' are [ 'ཀ', 'ག', 'ང', 'ཅ', 'ཇ', 'ཏ', 'ད', 'པ', 'བ', 'ཧ' ]
laSuperfixRoot :: HashSet Char
laSuperfixRoot = fetchChars subConsonants [1, 3, 4, 5, 7, 9, 11, 13, 15, 29]

pLaSuperfixRoot :: Parser Char
pLaSuperfixRoot =
    satisfy (`member` laSuperfixRoot)
        <?> "Superfix ལ should be placed above the root [ 'ཀ', 'ག', 'ང', 'ཅ', 'ཇ', 'ཏ', 'ད', 'པ', 'བ', 'ཧ' ]"

--
-- Roots under superfix 'ས' are [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'པ', 'བ', 'མ', 'ཙ' ]
saSuperfixRoot :: HashSet Char
saSuperfixRoot = fetchChars subConsonants [1, 3, 4, 8, 9, 11, 12, 13, 15, 16, 17]

pSaSuperfixRoot :: Parser Char
pSaSuperfixRoot =
    satisfy (`member` saSuperfixRoot)
        <?> "A root from [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'པ', 'བ', 'མ', 'ཙ' ] should be placed under superfix ས"
