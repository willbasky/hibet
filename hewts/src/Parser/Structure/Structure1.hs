module Parser.Structure.Structure1
    ( structure1
    ) where

import Data.Text (Text, pattern (:>))
import qualified Data.Text as T
import Parser.Common
import Text.Megaparsec

structure1 :: Parser Text
structure1 = parse_1

parse_1 :: Parser Text
parse_1 = do
    root <- pRootConsonant
    vowel <- optional $ choice [pVowel, vowelLongA]
    eof -- TODO: update syllable ending
    pure $ maybe (T.empty :> root) (\c -> T.empty :> root :> c) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_1 "སཱ"
-- Right "སཱ"

parse_1sanskrit :: Parser Text
parse_1sanskrit = do
    root <- pSanskrit
    vowel <- optional pVowel
    eof -- TODO: update syllable ending
    pure $ maybe (T.empty :> root) (\c -> T.empty :> root :> c) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_1sanskrit "ཊཱ"
-- Left "1:2:
--     |
--   1 | ཊཱ
--     |  ^
--   unexpected 'ཱ'
--   expecting <<explicit vowel character>> or end of input
--   "
