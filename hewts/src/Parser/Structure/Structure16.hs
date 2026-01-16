{-
Tibetan spelling structure 16
On the basis of the Tibetan spelling grammar 4.13, 4.15, 4.16
-}

-- module Parser.Structure.Structure16 (pStructure16) where

-- import Parser.Common
-- import Parser.Structure.Grammar13 (pGrammar13)
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

-- pStructure16 :: Parser Text
-- pStructure16 = do
--     struct <-
--         choice
--             [ try $ parse16 pGrammar16Da pPostfixDa
--             , try $ parse16 pGrammar16Sa pPostfixSa
--             ]
--     eof
--     pure struct

-- parse16 :: Parser Char -> Parser Char -> Parser Text
-- parse16 parseSuffix parsePostfix = do
--     struct <- pGrammar13
--     suffix <- parseSuffix
--     postfix <- parsePostfix
--     pure $ struct :> suffix :> postfix

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure16 "བསྒྱུགས"
-- Right "བསྒྱུགས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pStructure16 "བརྐྱནད"
-- Right "བརྐྱནད"
