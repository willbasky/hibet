{-
Tibetan spelling grammar 4.17
-}

module Convert.Grammar.Rules.Grammar17
    ( pGrammar17
    ) where

import Convert.Grammar.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import qualified Data.Text as T
import Data.Text (Text, pattern (:<), pattern (:>))
import Text.Megaparsec
import Text.Megaparsec.Char

pGrammar17 :: Parser Text
pGrammar17 =
    choice
        [ try $ parse17 pRoot1 pSubfixRa
        , try $ parse17 pRoot2 pSubfixYa
        ]

parse17 :: Parser Char -> Parser Char -> Parser Text
parse17 parseRoot parseSubfix = do
    root <- parseRoot
    subfix <- parseSubfix
    wa <- pSubfixWa
    vowel <- optional pVowel
    let consT = T.empty :> root :> subfix :> wa
    pure $ maybe consT (consT :>) vowel

--
-- (1) root group [ 'ག', 'ད' ] above the subfix ར.
roots1 :: HashSet Char
roots1 = fetchChars consonants [3,11]

pRoot1 :: Parser Char
pRoot1 =
    satisfy (`member` roots1)
        <?> "A root from [ 'ག', 'ད' ]"

-- ཕ
pRoot2 :: Parser Char
pRoot2 = char (fetchChar consonants 14) <?> "A root ཕ"