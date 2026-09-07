{-
Tibetan spelling structure 20
On the basis of the Tibetan spelling grammar 4.10 and 4.15
-}

-- module Parser.Structure.Structure20 (pStructure20) where

-- import Parser.Common
-- import Parser.Structure.Grammar15 (pGrammar15)
-- import Parser.Structure.Grammar10 (pGrammar10)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure20 :: Parser Text
-- pStructure20 = do
--     struct <- pGrammar10
--     suffix <- pGrammar15
--     eof
--     pure $ struct :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure20 "སྐྲོན"
-- Right "སྐྲོན"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure20 "སྒྱར"
-- Right "སྒྱར"
