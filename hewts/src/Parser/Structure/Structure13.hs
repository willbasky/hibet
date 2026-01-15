{-
Tibetan spelling structure 13
On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
-}

module Parser.Structure.Structure13
    ( pStructure13
    , pSuffixPostfixDaGrammar16
    , pSuffixPostfixSaGrammar16
    , 
    ) where

import Parser.Common
import Parser.Structure.Structure8 (pGrammar14)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure13 :: Parser Text
pStructure13 = do
    struct <- choice
        [ try $ parse13 pSuffixPostfixDaGrammar16 pPostfixDa
        , try $ parse13 pSuffixPostfixSaGrammar16 pPostfixSa
        ]
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure13 "བཏནད"
-- Right "བཏནད"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure13 "གཏིགས"
-- Right "གཏིགས"

parse13 :: Parser Char -> Parser Char -> Parser Text
parse13 parseSuffix parsePostfix = do
    struct <- pGrammar14
    suffix <- parseSuffix
    postfix <- parsePostfix
    pure $ struct :> suffix :> postfix

-- Tibetan spelling grammar 4.16

-- 
-- Suffix group [ 'ན', 'ར', 'ལ' ] before postfix ད.
suffixesPostfixDa :: HashSet Char
suffixesPostfixDa = fetchChars consonants [12, 25, 26]

pSuffixPostfixDaGrammar16 :: Parser Char
pSuffixPostfixDaGrammar16 =
    satisfy (`member` suffixesPostfixDa)
        <?> "A suffix from [ 'ན', 'ར', 'ལ' ]"

-- Suffix group [ 'ག', 'ང', 'བ', 'མ' ] before postfix ས.
suffixesPostfixSa :: HashSet Char
suffixesPostfixSa = fetchChars consonants [3, 4, 15, 16]

pSuffixPostfixSaGrammar16 :: Parser Char
pSuffixPostfixSaGrammar16 =
    satisfy (`member` suffixesPostfixSa)
        <?> "A suffix from [ 'ག', 'ང', 'བ', 'མ' ]"
