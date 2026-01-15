{-
Tibetan spelling structure 10
On the basis of the Tibetan spelling grammar 4.11 and 4.15
-}

module Parser.Structure.Structure10 (pStructure10) where

import Parser.Common
import Parser.Structure.Structure5 (pStructure11, pStructureConsonants11)
import Parser.Structure.Structure9 (pSuffix15)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure10 :: Parser Text
pStructure10 = do 
    structure8 <- pStructureConsonants11 <|> pStructure11
    suffix <- pSuffix15
    pure $ structure8 :> suffix