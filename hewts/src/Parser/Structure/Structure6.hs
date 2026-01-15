{-
Tibetan spelling structure 6
On the basis of the Tibetan spelling grammar 4.12
-}

module Parser.Structure.Structure6
    ( pGrammar12
    , pStructure6
    ) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure6 :: Parser Text
pStructure6 = do
    struct <- pGrammar12
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure6 "འདྲ"
-- Right "འདྲ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure6 "མགྱ"
-- Right "མགྱ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure6 "མགྲ"
-- Right "མགྲ"

-- Tibetan spelling grammar 4.12
pGrammar12 :: Parser Text
pGrammar12 =
    choice
        [ try $ parse12 pPrefixDa pRoot1 pSubfixYa
        , try $ parse12 pPrefixDa pRoot2 pSubfixRa
        , try $ parse12 pPrefixBa pRoot3 pSubfixYa
        , try $ parse12 pPrefixBa pRoot4 pSubfixRa
        , try $ parse12 pPrefixBa pRoot5 pSubfixLa
        , try $ parse12 pPrefixMa pRoot6 (pSubfixYa <|> pSubfixRa)
        , try $ parse12 pPrefixA pRoot7 pSubfixYa
        , try $ parse12 pPrefixA pRoot8 pSubfixRa
        ]

parse12 :: Parser Char -> Parser Char -> Parser Char -> Parser Text
parse12 parsePrefix parseRoot parseSubfix = do
    prefix <- parsePrefix
    root <- parseRoot
    subfix <- parseSubfix
    vowel <- optional pVowel
    let consT = T.empty :> prefix :> root :> subfix
    pure $ maybe consT (consT :>) vowel

--
-- (1) root group [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ] with prefix ད under subfix ཡ.
roots1 :: HashSet Char
roots1 = fetchChars consonants [1, 3, 13, 15, 16]

pRoot1 :: Parser Char
pRoot1 =
    satisfy (`member` roots1)
        <?> "A root from [ 'ཀ', 'ག', 'པ', 'བ', 'མ' ]"

--
-- (2) root group [ 'ཀ', 'ག', 'པ', 'བ' ] with prefix ད under subfix ཡ.
roots2 :: HashSet Char
roots2 = fetchChars consonants [1, 3, 13, 15]

pRoot2 :: Parser Char
pRoot2 =
    satisfy (`member` roots2)
        <?> "A root from [ 'ཀ', 'ག', 'པ', 'བ' ]"

--
-- (3) root group [ 'ཀ', 'ག' ] with prefix བ under subfix ཡ.
roots3 :: HashSet Char
roots3 = fetchChars consonants [1, 3]

pRoot3 :: Parser Char
pRoot3 =
    satisfy (`member` roots3)
        <?> "A root from [ 'ཀ', 'ག' ]"

--
-- (4) root group [ 'ཀ', 'ག', 'ས' ] with prefix བ under subfix ར.
roots4 :: HashSet Char
roots4 = fetchChars consonants [1, 3, 28]

pRoot4 :: Parser Char
pRoot4 =
    satisfy (`member` roots4)
        <?> "A root from [ 'ཀ', 'ག', 'ས' ]"

--
-- (5) root group [ 'ཀ', 'ཟ', 'ར', 'ས' ] with prefix བ under subfix ལ.
roots5 :: HashSet Char
roots5 = fetchChars consonants [1, 22, 25, 28]

pRoot5 :: Parser Char
pRoot5 =
    satisfy (`member` roots5)
        <?> "A root from [ 'ཀ', 'ཟ', 'ར', 'ས' ]"

--
-- (6) root group [ 'ཁ', 'ག' ] with prefix མ under subfix ཡ or ར.
roots6 :: HashSet Char
roots6 = fetchChars consonants [2, 3]

pRoot6 :: Parser Char
pRoot6 =
    satisfy (`member` roots6)
        <?> "A root from [ 'ཁ', 'ག' ]"

--
-- (7) root group [ 'ཁ', 'ག', 'ཕ', 'བ' ] with prefix འ under subfix ཡ.
roots7 :: HashSet Char
roots7 = fetchChars consonants [2, 3, 14, 15]

pRoot7 :: Parser Char
pRoot7 =
    satisfy (`member` roots7)
        <?> "A root from [ 'ཁ', 'ག', 'ཕ', 'བ' ]"

--
-- (8) root group [ 'ཁ', 'ག', 'ད', 'ཕ', 'བ' ] with prefix འ under subfix ར.
roots8 :: HashSet Char
roots8 = fetchChars consonants [2, 3, 11, 14, 15]

pRoot8 :: Parser Char
pRoot8 =
    satisfy (`member` roots8)
        <?> "A root from [ 'ཁ', 'ག', 'ད', 'ཕ', 'བ' ]"
