{-
Tibetan spelling structure 5
On the basis of the Tibetan spelling grammar 4.11
-}

module Parser.Structure.Structure05
    ( pStructure5
    ) where

import Parser.Common
import Parser.Structure.Grammar11

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure5 :: Parser Text
pStructure5 = do
    struct <- pGrammar11
    eof
    pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure5 "བལྟ"
-- Right "བལྟ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure5 "བརྒ"
-- Right "བརྒ"


