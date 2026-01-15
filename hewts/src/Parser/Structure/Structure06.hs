{-
Tibetan spelling structure 6
On the basis of the Tibetan spelling grammar 4.12
-}

module Parser.Structure.Structure06
    ( pStructure6
    ) where

import Parser.Common
import Parser.Structure.Grammar12 (pGrammar12)

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


