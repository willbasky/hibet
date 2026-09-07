{-
Tibetan spelling structure 12
On the basis of the Tibetan spelling grammar 4.13 and 4.15
-}

-- module Parser.Structure.Structure12 (pStructure12) where

-- import Parser.Common
-- import Parser.Structure.Grammar13 (pGrammar13)
-- import Parser.Structure.Grammar15 (pGrammar15)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure12 :: Parser Text
-- pStructure12 = do 
--     structure8 <- pGrammar13
--     suffix <- pGrammar15
--     pure $ structure8 :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བརྒྱས"
-- Right "བརྒྱས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བསྒྲས"
-- Right "བསྒྲས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བསྒྲོས"
-- Right "བསྒྲོས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure12 "བསྒྱས"
-- Right "བསྒྱས"
