{-
Tibetan spelling structure 6
On the basis of the Tibetan spelling grammar 4.12
-}

module Parser.Structure.Structure6 where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure6 :: Parser Text
pStructure6 =
    choice
        [ parse_6_1
        , parse_6_2
        , parse_6_3
        , parse_6_4
        , parse_6_5
        , parse_6_6
        , parse_6_7
        , parse_6_8
        ]

--
-- (1) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] with prefix ད under subfix ཡ.
roots1 :: HashSet Char
roots1 = fetchChars consonants [1, 3, 13, 15, 16]

pRoot1 :: Parser Char
pRoot1 =
    satisfy (`member` roots1)
        <?> "A root from [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ]"

parse_6_1 :: Parser Text
parse_6_1 = do
    prefix <- pPrefixDa
    root <- pRoot1
    subfix <- pSubfixYa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ' ] with prefix ད under subfix ཡ.
roots2 :: HashSet Char
roots2 = fetchChars consonants [1, 3, 13, 15]

pRoot2 :: Parser Char
pRoot2 =
    satisfy (`member` roots2)
        <?> "A root from [ 'ཀ', 'ག', 'པ', 'བ' ]"

parse_6_2 :: Parser Text
parse_6_2 = do
    prefix <- pPrefixDa
    root <- pRoot2
    subfix <- pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (3) root group [ 'ཀ', 'ག' ] with prefix བ under subfix ཡ.
roots3 :: HashSet Char
roots3 = fetchChars consonants [1, 3]

pRoot3 :: Parser Char
pRoot3 =
    satisfy (`member` roots3)
        <?> "A root from [ 'ཀ', 'ག' ]"

parse_6_3 :: Parser Text
parse_6_3 = do
    prefix <- pPrefixBa
    root <- pRoot3
    subfix <- pSubfixYa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (4) root group [ 'ཀ', 'ག', 'ས' ] with prefix བ under subfix ར.
roots4 :: HashSet Char
roots4 = fetchChars consonants [1, 3, 28]

pRoot4 :: Parser Char
pRoot4 =
    satisfy (`member` roots4)
        <?> "A root from [ 'ཀ', 'ག', 'ས' ]"

parse_6_4 :: Parser Text
parse_6_4 = do
    prefix <- pPrefixBa
    root <- pRoot4
    subfix <- pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (5) root group [ 'ཀ', 'ཟ', 'ར', 'ས' ] with prefix བ under subfix ལ.
roots5 :: HashSet Char
roots5 = fetchChars consonants [1, 22, 25, 28]

pRoot5 :: Parser Char
pRoot5 =
    satisfy (`member` roots5)
        <?> "A root from [ 'ཀ', 'ཟ', 'ར', 'ས' ]"

parse_6_5 :: Parser Text
parse_6_5 = do
    prefix <- pPrefixBa
    root <- pRoot5
    subfix <- pSubfixLa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (6) root group [ 'ཁ', 'ག' ] with prefix མ under subfix ཡ or ར.
roots6 :: HashSet Char
roots6 = fetchChars consonants [2, 3]

pRoot6 :: Parser Char
pRoot6 =
    satisfy (`member` roots6)
        <?> "A root from [ 'ཁ', 'ག' ]"

parse_6_6 :: Parser Text
parse_6_6 = do
    prefix <- pPrefixMa
    root <- pRoot6
    subfix <- pSubfixYa <|> pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (7) root group [ 'ཁ', 'ག', 'ཕ', 'བ' ] with prefix འ under subfix ཡ.
roots7 :: HashSet Char
roots7 = fetchChars consonants [2, 3, 14, 15]

pRoot7 :: Parser Char
pRoot7 =
    satisfy (`member` roots7)
        <?> "A root from [ 'ཁ', 'ག', 'ཕ', 'བ' ]"

parse_6_7 :: Parser Text
parse_6_7 = do
    prefix <- pPrefixA
    root <- pRoot7
    subfix <- pSubfixYa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (8) root group [ 'ཁ', 'ག', 'ད', 'ཕ', 'བ' ] with prefix འ under subfix ར.
roots8 :: HashSet Char
roots8 = fetchChars consonants [2, 3, 11, 14, 15]

pRoot8 :: Parser Char
pRoot8 =
    satisfy (`member` roots8)
        <?> "A root from [ 'ཁ', 'ག', 'ད', 'ཕ', 'བ' ]"

parse_6_8 :: Parser Text
parse_6_8 = do
    prefix <- pPrefixA
    root <- pRoot8
    subfix <- pSubfixRa
    vowel <- optional pVowel
    eof
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_6_8 "འདྲ"
-- Right "འདྲ"
