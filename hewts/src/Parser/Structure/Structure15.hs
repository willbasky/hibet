{-
Tibetan spelling structure 15
On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
-}

module Parser.Structure.Structure15 (pStructure15) where

import Parser.Common
import Parser.Structure.Grammar13 (pGrammar13)
import Parser.Structure.Grammar16
    ( pGrammar16Da
    , pGrammar16Sa
    )

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure15 :: Parser Text
pStructure15 = do
    struct <-
        choice
            [ try $ parse15 pGrammar16Da pPostfixDa
            , try $ parse15 pGrammar16Sa pPostfixSa
            ]
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure15 "བརྒྱརད"
-- Right "བརྒྱརད"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure15 "བརྐྱམས"
-- Right "བརྐྱམས"

parse15 :: Parser Char -> Parser Char -> Parser Text
parse15 parseSuffix parsePostfix = do
    struct <- pGrammar13
    suffix <- parseSuffix
    postfix <- parsePostfix
    pure $ struct :> suffix :> postfix
