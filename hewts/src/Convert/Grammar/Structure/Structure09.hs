{-
Tibetan spelling structure 9
On the basis of the Tibetan spelling grammar 4.14 and 4.15
-}

-- module Parser.Structure.Structure09
--     ( pStructure9
--     ) where

-- import Parser.Common
-- import Parser.Structure.Grammar14 (pGrammar14)
-- import Parser.Structure.Grammar15 (pGrammar15)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure9 :: Parser Text
-- pStructure9 = do
--     struct <- pGrammar14
--     suffix <- pGrammar15
--     pure $ struct :> suffix

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


