{-
Tibetan spelling structure 8
On the basis of the Tibetan spelling grammar 4.14
-}

-- module Parser.Structure.Structure08
--     ( pStructure8
--     ) where

-- import Parser.Common
-- import Parser.Structure.Grammar14

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure8 :: Parser Text
-- pStructure8 = do
--     struct <- pGrammar14
--     eof
--     pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure8 "བཏ"
-- Right "བཏ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure8 "གཏ"
-- Right "གཏ"


