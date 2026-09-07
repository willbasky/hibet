{-
Tibetan spelling structure 14
On the basis of the Tibetan spelling grammar 4.11, 4.15, 4.16
-}

-- module Parser.Structure.Structure14 (pStructure14) where

-- import Parser.Common
-- import Parser.Structure.Grammar11 (pGrammar11)
-- import Parser.Structure.Grammar16
--     ( pGrammar16Da
--     , pGrammar16Sa
--     )

-- import Data.Char (chr)
-- import Data.HashSet (HashSet, fromList, member, singleton)
-- import Data.Text (Text, pattern (:<), pattern (:>))
-- import qualified Data.Text as T
-- import Text.Megaparsec
-- import Text.Megaparsec.Char

-- pStructure14 :: Parser Text
-- pStructure14 = do
--     struct <-
--         choice
--             [ try $ parse14 pGrammar16Da pPostfixDa
--             , try $ parse14 pGrammar16Sa pPostfixSa
--             ]
--     eof
--     pure struct

-- parse14 :: Parser Char -> Parser Char -> Parser Text
-- parse14 parseSuffix parsePostfix = do
--     struct <- pGrammar11
--     suffix <- parseSuffix
--     postfix <- parsePostfix
--     pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure14 "བརྟགས"
-- Right "བརྟགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure14 "བརྟིབས"
-- Right "བརྟིབས"
