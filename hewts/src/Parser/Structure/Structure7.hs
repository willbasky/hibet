{-
Tibetan spelling structure 7
On the basis of the Tibetan spelling grammar 4.13
-}

module Parser.Structure.Structure7 (pStructure7) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure7 :: Parser Text
pStructure7 =
    choice
        [ parse_7_1
        , parse_7_2
        ]

--
-- root group [ 'ཀ', 'ག' ]
roots :: HashSet Char
roots = fetchChars subConsonants [1, 3]

pRoot :: Parser Char
pRoot =
    satisfy (`member` roots)
        <?> "A root from [ 'ཀ', 'ག' ]"

parse_7_1 :: Parser Text
parse_7_1 = do
    prefix <- pPrefixBa
    superfix <- pSuperfixSa
    root <- pRoot
    subfix <- pSubfixYa <|> pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

parse_7_2 :: Parser Text
parse_7_2 = do
    prefix <- pPrefixBa
    superfix <- pSuperfixRa
    root <- pRoot
    subfix <- pSubfixYa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_7_2 "བརྒྱ"
-- Right "བརྒྱ"
