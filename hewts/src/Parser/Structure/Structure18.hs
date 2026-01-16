{-
Tibetan spelling structure 18
On the basis of the Tibetan spelling grammar 4.8 and 4.15
-}

-- module Parser.Structure.Structure18 (pStructure18) where

-- import Parser.Common
-- import Parser.Structure.Grammar15 (pGrammar15)
-- import Parser.Structure.Grammar08 (pGrammar8)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure18 :: Parser Text
-- pStructure18 = do
--     struct <- pGrammar8
--     suffix <- pGrammar15
--     eof
--     pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure18 "རྒིད"
-- Right "རྒིད"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure18 "སྒོལ"
-- Right "སྒོལ"
