{-
Tibetan spelling structure 1
On the basis of the Tibetan spelling grammar 4.1
-}

-- module Parser.Structure.Structure01
--     ( pStructure1
--     ) where

-- import Parser.Common
-- import Parser.Rules.Grammar01 (pGrammar1)

-- import Data.Text (Text, pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec

-- pStructure1 :: Parser Text
-- pStructure1 = do
--     struct <- pGrammar1 
--     eof -- TODO: update syllable ending
--     pure struct

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "སཱ"
-- Right "སཱ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "ས"
-- Right "ས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "སོ"
-- Right "སོ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "ཊཱ"
-- Left "1:2:
--     |
--   1 | ཊཱ
--     |  ^
--   unexpected 'ཱ'
--   expecting Vowel character or end of input
--   "

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure1 "ཊོ"
-- Right "ཊོ"
