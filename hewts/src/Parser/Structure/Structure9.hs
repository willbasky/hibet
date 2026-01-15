{-
Tibetan spelling structure 9
On the basis of the Tibetan spelling grammar 4.14 and 4.15
-}

module Parser.Structure.Structure9 (pStructure9) where

import Parser.Common
import Parser.Structure.Structure8 (pStructure8)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure9 :: Parser Text
pStructure9 = do 
    structure8 <- pStructure8
    suffix <- pSuffix9
    pure $ structure8 :> suffix

--
-- suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ] .
suffix9 :: HashSet Char
suffix9 = fetchChars consonants [3, 4, 11, 12, 15, 16, 23, 25, 26, 28]

pSuffix9 :: Parser Char
pSuffix9 =
    satisfy (`member` suffix9)
        <?> "A suffix from [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ]"

