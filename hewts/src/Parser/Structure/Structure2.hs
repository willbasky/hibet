module Parser.Structure.Structure2 
    ( structure2
    ) where

import Data.Text (Text, pattern (:>), pattern (:<))
import qualified Data.Text as T
import Parser.Common
import Text.Megaparsec
import Data.HashSet (HashSet, fromList, member, singleton)
import Text.Megaparsec.Char

structure2 :: Parser Text
structure2 = choice [parse_2_ra, parse_2_la, parse_2_sa]

--
-- Roots superfix 'ར' [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]
raSuperfixRoot :: HashSet Char
raSuperfixRoot = fetchChars consonantsInSubPosition [1, 3, 4, 7, 8, 9, 11, 12, 15, 16, 17, 19]

pRa :: Parser Char 
pRa = char $ fetchChar 25

pRaSuperfixRoot :: Parser Char
pRaSuperfixRoot = satisfy (`member` raSuperfixRoot) <?> "<<Superscript ར must be followed by a valid root consonant [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]>>"

parse_2_ra :: Parser Text
parse_2_ra = do
    prefix <- pRa
    root <- pRaSuperfixRoot
    vowel <- optional pVowel
    eof
    let pref = T.empty :> prefix
    pure $ maybe (pref :> root) (\c -> pref :> root :> c) vowel

--
-- Roots after superfix 'ལ' [ 'ཀ', 'ག', 'ང', 'ཅ', 'ཇ', 'ཏ', 'ད', 'པ', 'བ', 'ཧ' ]
laSuperfixRoot :: HashSet Char
laSuperfixRoot = fetchChars consonantsInSubPosition [1, 3, 4, 5, 7, 9, 11, 13, 15, 29]

pLa :: Parser Char 
pLa = char $ fetchChar 26

pLaSuperfixRoot :: Parser Char
pLaSuperfixRoot = satisfy (`member` laSuperfixRoot) <?> "<<Superscript ལ must be followed by a valid root consonant [ 'ཀ', 'ག', 'ང', 'ཅ', 'ཇ', 'ཏ', 'ད', 'པ', 'བ', 'ཧ' ]>>"

parse_2_la :: Parser Text
parse_2_la = do
    prefix <- pLa
    root <- pLaSuperfixRoot
    vowel <- optional pVowel
    eof
    let pref = T.empty :> prefix
    pure $ maybe (pref :> root) (\c -> pref :> root :> c) vowel
    
-- 
-- Roots after superfix 'ས' [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'པ', 'བ', 'མ', 'ཙ' ]
saSuperfixRoot :: HashSet Char
saSuperfixRoot = fetchChars consonantsInSubPosition [1, 3, 4, 8, 9, 11, 12, 13, 15, 16, 17]

pSa :: Parser Char 
pSa = char $ fetchChar 28

pSaSuperfixRoot :: Parser Char
pSaSuperfixRoot = satisfy (`member` laSuperfixRoot) <?> "<<Superscript ས must be followed by a valid root consonant [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'པ', 'བ', 'མ', 'ཙ' ]>>"

parse_2_sa :: Parser Text
parse_2_sa = do
    prefix <- pSa
    root <- pSaSuperfixRoot
    vowel <- optional pVowel
    eof
    let pref = T.empty :> prefix
    pure $ maybe (pref :> root) (\c -> pref :> root :> c) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_2_sa "སྒ"
-- Right "སྒ"
