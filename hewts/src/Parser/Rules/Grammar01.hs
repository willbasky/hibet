{-
Tibetan spelling grammar 4.1
-}

module Parser.Rules.Grammar01 (pGrammar1, pGrammar1WithLong, pGrammar1Sanskrit) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.1

pGrammar1 :: Parser Text 
pGrammar1 = do 
    root <- pRootConsonant
    vowel <- optional pVowel 
    let consT = T.empty :> root 
    pure $ maybe consT (consT :>) vowel

pGrammar1WithLong :: Parser Text 
pGrammar1WithLong = do 
    root <- pRootConsonant
    vowel <- optional $ choice [pVowel, vowelLongA] 
    let consT = T.empty :> root 
    pure $ maybe consT (consT :>) vowel

pGrammar1Sanskrit :: Parser Text 
pGrammar1Sanskrit = do 
    root <- pSanskrit
    vowel <- optional pVowel 
    let consT = T.empty :> root 
    pure $ maybe consT (consT :>) vowel