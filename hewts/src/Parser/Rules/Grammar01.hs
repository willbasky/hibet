{-
Tibetan spelling grammar 4.1
-}

module Parser.Rules.Grammar01 (pGrammar1) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.1

pGrammar1 :: Parser Text 
pGrammar1 =  
    choice
        [ try $ parseGrammar1 pRootConsonant $ choice [pVowel, vowelLongA]
        , try $ parseGrammar1 pSanskrit pVowel
        ]

parseGrammar1 :: Parser Char -> Parser Char -> Parser Text
parseGrammar1 parseRoot parseVowel = do
    root <- parseRoot
    vowel <- optional parseVowel
    let consT = T.empty :> root
    pure $ maybe consT (consT :>) vowel