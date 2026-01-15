{-
Tibetan spelling structure 13
On the basis of the Tibetan spelling grammar 4.14, 4.15, 4.16
-}

module Parser.Structure.Structure13
    ( pStructure13
    ) where

import Parser.Common
import Parser.Structure.Grammar14 (pGrammar14)
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

pStructure13 :: Parser Text
pStructure13 = do
    struct <-
        choice
            [ try $ parse13 pGrammar16Da pPostfixDa
            , try $ parse13 pGrammar16Sa pPostfixSa
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
