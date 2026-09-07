{-
Tibetan spelling structure 3
On the basis of the Tibetan spelling grammar 4.9
-}

-- module Parser.Structure.Structure03 (pStructure3) where

-- import Parser.Common
-- import Parser.Structure.Grammar09 (pGrammar9)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure3 :: Parser Text
-- pStructure3 = do
--     struct <- pGrammar9
--     eof 
--     pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure3 "སྲོ"
-- Right "སྲོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure3 "སླ"
-- Right "སླ"

