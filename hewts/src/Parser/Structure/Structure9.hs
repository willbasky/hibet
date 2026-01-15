{-
Tibetan spelling structure 9
On the basis of the Tibetan spelling grammar 4.14 and 4.15
-}

module Parser.Structure.Structure9
    ( pStructure9
    , pSuffixGrammar15
    ) where

import Parser.Common
import Parser.Structure.Structure8 (pGrammar14)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure9 :: Parser Text
pStructure9 = do
    struct <- pGrammar14
    suffix <- pSuffixGrammar15
    pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure9 "བཏག"
-- Right "བཏག"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure9 "བཏིག"
-- Right "བཏིག"

--
-- Suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ] .
suffix15 :: HashSet Char
suffix15 = fetchChars consonants [3, 4, 11, 12, 15, 16, 23, 25, 26, 28]

pSuffixGrammar15 :: Parser Char
pSuffixGrammar15 =
    satisfy (`member` suffix15)
        <?> "A suffix from [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ]"
