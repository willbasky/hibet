{-
Tibetan spelling structure 1
On the basis of the Tibetan spelling grammar 4.1
-}

module Parser.Structure.Structure01
    ( pStructure1
    ) where

import Data.Text (Text, pattern (:>))
import qualified Data.Text as T
import Parser.Common
import Text.Megaparsec

pStructure1 :: Parser Text
pStructure1 = parse_1

parse_1 :: Parser Text
parse_1 = do
    root <- pRootConsonant
    vowel <- optional $ choice [pVowel, vowelLongA]
    eof -- TODO: update syllable ending
    let consT = T.empty :> root
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_1 "སཱ"
-- Right "སཱ"

parse_1sanskrit :: Parser Text
parse_1sanskrit = do
    root <- pSanskrit
    vowel <- optional pVowel
    eof
    let consT = T.empty :> root
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_1sanskrit "ཊཱ"
-- Left "1:2:
--     |
--   1 | ཊཱ
--     |  ^
--   unexpected 'ཱ'
--   expecting Vowel character or end of input
--   "
