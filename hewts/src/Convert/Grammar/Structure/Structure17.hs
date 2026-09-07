{-
Tibetan spelling structure 17
On the basis of the Tibetan spelling grammar 4.15
-}

-- module Parser.Structure.Structure17 (pStructure17) where

-- import Parser.Common
-- import Parser.Structure.Grammar15 (pGrammar15)

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure17 :: Parser Text
-- pStructure17 = do
--     root <- pRootConsonant
--     vowel <- optional pVowel
--     suffix <- pGrammar15
--     eof
--     let consT = T.empty :> root
--     let consVowelT = maybe consT (consT :>) vowel
--     pure $ consVowelT :> suffix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure17 "བིག"
-- Right "བིག"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure17 "བས"
-- Right "བས"
