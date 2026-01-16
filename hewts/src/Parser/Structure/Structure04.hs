{-
Tibetan spelling structure 4
On the basis of the Tibetan spelling grammar 4.10
-}

-- module Parser.Structure.Structure04 (pStructure4) where

-- import Parser.Common
-- import Parser.Structure.Grammar10 (pGrammar10)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure4 :: Parser Text
-- pStructure4 = do 
--     struct <- pGrammar10 
--     eof 
--     pure struct 

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "རྐྱུ"
-- Right "རྐྱུ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "སྐྲོ"
-- Right "སྐྲོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "སྒྲ"
-- Right "སྒྲ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure4 "སྤྱ"
-- Right "སྤྱ"