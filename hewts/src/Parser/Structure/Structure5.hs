{-
Tibetan spelling structure 5
On the basis of the Tibetan spelling grammar 4.11
-}

module Parser.Structure.Structure5 (structure5) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

structure5 :: Parser Text
structure5 = choice [parse_5_1, parse_5_2, parse_5_3]

--
-- (1) root group [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཛ' ] with prefix བ under superfix ར.
roots1 :: HashSet Char
roots1 = fetchChars subConsonants [1, 3, 4, 7, 8, 9, 11, 12, 17, 19]

pRoot1 :: Parser Char
pRoot1 =
    satisfy (`member` roots1)
        <?> "A root from [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ', 'ཛ' ]"

parse_5_1 :: Parser Text
parse_5_1 = do
    prefix <- pPrefixBa
    superfix <- pSuperfixRa
    root <- pRoot1
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> superfix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (2) root group [ 'ཏ', 'ད' ] with prefix བ under superfix ལ.
roots2 :: HashSet Char
roots2 = fetchChars subConsonants [9, 11]

pRoot2 :: Parser Char
pRoot2 = satisfy (`member` roots2) <?> "A root from [ 'ཏ', 'ད' ]"

parse_5_2 :: Parser Text
parse_5_2 = do
    prefix <- pPrefixBa
    superfix <- pSuperfixLa
    root <- pRoot2
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> superfix :> root
    pure $ maybe consT (consT :>) vowel

--
-- (3) root group [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ' ] with prefix བ under superfix ས.
roots3 :: HashSet Char
roots3 = fetchChars subConsonants [1, 3, 4, 8, 9, 11, 12, 17]

pRoot3 :: Parser Char
pRoot3 =
    satisfy (`member` roots3)
        <?> "A root from [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'ཙ' ]"

parse_5_3 :: Parser Text
parse_5_3 = do
    prefix <- pPrefixBa
    superfix <- pSuperfixSa
    root <- pRoot3
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> superfix :> root
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_5_3 "བསྒ"
-- Right "བསྒ"
