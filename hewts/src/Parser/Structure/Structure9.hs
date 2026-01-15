{-
Tibetan spelling structure 9
On the basis of the Tibetan spelling grammar 4.14 and 4.15
-}

module Parser.Structure.Structure9
    ( pStructure9
    , pSuffix15
    ) where

import Parser.Common
import Parser.Structure.Structure8 (pStructure14, pStructureConsonants14)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure9 :: Parser Text
pStructure9 = do
    struct <- pStructureConsonants14 <|> pStructure14
    suffix <- pSuffix15
    pure $ struct :> suffix

--
-- Suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ] .
suffix15 :: HashSet Char
suffix15 = fetchChars consonants [3, 4, 11, 12, 15, 16, 23, 25, 26, 28]

pSuffix15 :: Parser Char
pSuffix15 =
    satisfy (`member` suffix15)
        <?> "A suffix from [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ]"
