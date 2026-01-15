{-
Tibetan spelling structure 4
On the basis of the Tibetan spelling grammar 4.10
-}
module Parser.Structure.Structure4 (structure4) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

structure4 :: Parser Text
structure4 = choice [parse_4_1, parse_4_2, parse_4_3, parse_4_4]

--
-- (1) root group [ 'ཀ', 'ག', 'མ' ] under superfix ར and above subfix ཡ. 
roots1 :: HashSet Char
roots1 = fetchChars subConsonants [1, 3, 16]

pRoot1 :: Parser Char
pRoot1 = satisfy (`member` roots1) <?> "<<roots [ 'ཀ', 'ག', 'མ' ]>>"

parse_4_1 :: Parser Text
parse_4_1 = do
    superfix <- pSuperfixRa
    root <- pRoot1
    subfix <- pSubfixYa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] under superfix ས and above subfix ཡ and ར. 
roots2 :: HashSet Char
roots2 = fetchChars subConsonants [1, 3, 13, 15, 16]

pRoots2 :: Parser Char
pRoots2 = satisfy (`member` roots2) <?> "<<roots [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ]>>"

parse_4_2 :: Parser Text
parse_4_2 = do
    superfix <- pSuperfixSa
    root <- pRoots2
    subfix <- pSubfixYa <|> pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_4_2 "སྒྲ"
-- Right "སྒྲ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_4_2 "སྤྲ"
-- Right "སྤྲ"

parse_4_3 :: Parser Text
parse_4_3 = do
    superfix <- pSuperfixSa
    root <- char $ fetchChar subConsonants 12
    subfix <- pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

parse_4_4 :: Parser Text
parse_4_4 = do
    superfix <- pSuperfixRa
    root <- char $ fetchChar subConsonants 17
    subfix <- pSubfixWa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root :> subfix
    pure $ maybe consT (consT :>) vowel


