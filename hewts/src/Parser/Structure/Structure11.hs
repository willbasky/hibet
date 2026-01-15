{-
Tibetan spelling structure 11
On the basis of the Tibetan spelling grammar 4.12 and 4.15
-}

module Parser.Structure.Structure11 (pStructure11) where

import Parser.Common
import Parser.Structure.Grammar12 (pGrammar12)
import Parser.Structure.Grammar15 (pGrammar15)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure11 :: Parser Text
pStructure11 = do 
    structure8 <- pGrammar12
    suffix <- pGrammar15
    pure $ structure8 :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྱས"
-- Right "མགྱས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྱས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྲས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure11 "མགྲིས"
-- Right "མགྲིས"
