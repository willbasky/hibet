{-
Tibetan spelling structure 2
On the basis of the Tibetan spelling grammar 4.8
-}

-- module Parser.Structure.Structure02
--     ( pStructure2
--     ) where

-- import Parser.Common
-- import Parser.Structure.Grammar08

-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure2 :: Parser Text
-- pStructure2 = do
--     struct <- pGrammar8
--     eof
--     pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure2 "སྒ"
-- Right "སྒ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure2 "སྒ"
-- Right "སྒ"
