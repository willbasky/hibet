{-
Tibetan spelling structure 3
On the basis of the Tibetan spelling grammar 4.9
-}

module Parser.Structure.Structure3 (structure3) where

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Parser.Common
import Text.Megaparsec
import Text.Megaparsec.Char

structure3 :: Parser Text
structure3 = choice [parse_3_wa, parse_3_ya, parse_3_ra, parse_3_la]

--
-- Roots above subfix 'ཝ' are [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]
rootWaSubfix :: HashSet Char
rootWaSubfix = fetchChars consonants [1, 2, 3, 8, 11, 18, 21, 22, 25, 26, 27, 29]

pRootSubfixWa :: Parser Char
pRootSubfixWa =
    satisfy (`member` rootWaSubfix)
        <?> "Subfix ཝ should be placed below the root [ 'ཀ', 'ཁ', 'ག', 'ཉ', 'ད', 'ཚ', 'ཞ', 'ཟ', 'ར', 'ལ', 'ཤ', 'ཧ' ]"

parse_3_wa :: Parser Text
parse_3_wa = do
    root <- pRootSubfixWa
    subfix <- pSubfixWa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- Roots above subfix 'ཡ' are [ 'ཀ', 'ཁ', 'ག', 'པ', 'ཕ', 'བ', 'མ' ]
rootSubfixYa :: HashSet Char
rootSubfixYa = fetchChars consonants [1, 2, 3, 13, 14, 15, 16]

pRootSubfixYa :: Parser Char
pRootSubfixYa =
    satisfy (`member` rootSubfixYa)
        <?> "Subfix ཡ should be placed below the root [ 'ཀ', 'ཁ', 'ག', 'པ', 'ཕ', 'བ', 'མ' ]"

parse_3_ya :: Parser Text
parse_3_ya = do
    root <- pRootSubfixYa
    subfix <- pSubfixYa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- Roots above subfix 'ར' are [ 'ཀ', 'ཁ', 'ག', 'ཏ', 'ཐ', 'ད', 'པ', 'ཕ', 'བ', 'མ', 'ས', 'ཧ' ]
rootSubfixRa :: HashSet Char
rootSubfixRa = fetchChars consonants [1, 2, 3, 9, 10, 11, 13, 14, 15, 16, 28, 29]

pRootSubfixRa :: Parser Char
pRootSubfixRa =
    satisfy (`member` rootSubfixRa)
        <?> "Subfix ར should be placed below the root [ 'ཀ', 'ཁ', 'ག', 'ཏ', 'ཐ', 'ད', 'པ', 'ཕ', 'བ', 'མ', 'ས', 'ཧ' ]"

parse_3_ra :: Parser Text
parse_3_ra = do
    root <- pRootSubfixRa
    subfix <- pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- Roots above subfix 'ལ' are [ 'ཀ', 'ག', 'བ', 'ཟ', 'ར', 'ས' ]
rootSubfixLa :: HashSet Char
rootSubfixLa = fetchChars consonants [1, 3, 15, 22, 25, 28]

pRootSubfixLa :: Parser Char
pRootSubfixLa =
    satisfy (`member` rootSubfixLa)
        <?> "Subfix ལ should be placed below the root [ 'ཀ', 'ག', 'བ', 'ཟ', 'ར', 'ས' ]"

parse_3_la :: Parser Text
parse_3_la = do
    root <- pRootSubfixLa
    subfix <- pSubfixLa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> root :> subfix
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_3_la "ཕླ"
-- Left "1:1:
--     |
--   1 | ཕླ
--     | ^
--   unexpected 'ཕ'
--   expecting Subfix ལ should be placed below the root [ 'ཀ', 'ག', 'བ', 'ཟ', 'ར', 'ས' ]
--   "
