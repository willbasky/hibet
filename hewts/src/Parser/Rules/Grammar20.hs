{-
Tibetan spelling grammar 4.20
-}

module Parser.Rules.Grammar20
    ( pGrammar20
    ) where

import Parser.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import qualified Data.Text as T
import Data.Text (Text, pattern (:<), pattern (:>))
import Text.Megaparsec
import Text.Megaparsec.Char

pGrammar20 :: Parser Text
pGrammar20 = do
    aRoot <- pRootA
    vowelA <- optional $ choice 
        [ pVowel
        , pSubRootNga
        , pSubRootMa
        ]
    let consT = T.empty :> aRoot
    pure $ maybe consT (consT :>) vowelA

--
-- A root འ
pRootA :: Parser Char
pRootA = char (fetchChar consonants 23) <?> "A root འ"

-- ང
pSubRootNga :: Parser Char
pSubRootNga = char (fetchChar subConsonants 4) <?> "A subConsonant ང"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pSubRootNga "བ"
-- Left "1:1:
--     |
--   1 | བ
--     | ^
--   unexpected 'བ'
--   expecting A subRoot ང
--   "

-- མ
pSubRootMa :: Parser Char
pSubRootMa = char (fetchChar subConsonants 16) <?> "A subConsonant མ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pSubRootMa "ྨ"
-- Right 'ྨ'

