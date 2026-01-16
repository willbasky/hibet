{-
Tibetan spelling structure 7
On the basis of the Tibetan spelling grammar 4.13
-}

-- module Parser.Structure.Structure07
--     ( pStructure7
--     ) where

-- import Parser.Common
-- import Parser.Structure.Grammar13 (pGrammar13)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure7 :: Parser Text
-- pStructure7 = do
--     struct <- pGrammar13
--     eof
--     pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure7 "བརྒྱ"
-- Right "བརྒྱ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure7 "བསྒྲ"
-- Right "བསྒྲ"


