{-
Tibetan spelling structure 10
On the basis of the Tibetan spelling grammar 4.11 and 4.15
-}

module Parser.Structure.Structure10 (pStructure10) where

import Parser.Common
import Parser.Structure.Structure5 (pGrammar11)
import Parser.Structure.Structure9 (pSuffixGrammar15)

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

pStructure10 :: Parser Text
pStructure10 = do 
    structure8 <- pGrammar11
    suffix <- pSuffixGrammar15
    eof
    pure $ structure8 :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure10 "བརྒན"
-- Right "བརྒན"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure10 "བརྒིག"
-- Right "བརྒིག"
