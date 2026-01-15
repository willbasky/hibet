{-
Tibetan spelling structure 2
On the basis of the Tibetan spelling grammar 4.8
-}

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
-- Roots under superfix 'ར' are [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]
raSuperfixRoot :: HashSet Char
raSuperfixRoot = fetchChars subConsonants [1, 3, 4, 7, 8, 9, 11, 12, 15, 16, 17, 19]

pRaSuperfixRoot :: Parser Char
pRaSuperfixRoot = satisfy (`member` raSuperfixRoot) <?> "<<Superfix ར should be placed above the root [ 'ཀ', 'ག', 'ང', 'ཇ', 'ཉ', 'ཏ', 'ད', 'ན', 'བ', 'མ', 'ཙ', 'ཛ' ]>>"

parse_2_ra :: Parser Text
parse_2_ra = do
    superfix <- pSuperfixRa
    root <- pRaSuperfixRoot
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root
    pure $ maybe consT (consT :>) vowel

--
-- Roots under superfix 'ལ' are [ 'ཀ', 'ག', 'ང', 'ཅ', 'ཇ', 'ཏ', 'ད', 'པ', 'བ', 'ཧ' ]
laSuperfixRoot :: HashSet Char
laSuperfixRoot = fetchChars subConsonants [1, 3, 4, 5, 7, 9, 11, 13, 15, 29]

pLaSuperfixRoot :: Parser Char
pLaSuperfixRoot = satisfy (`member` laSuperfixRoot) <?> "<<Superfix ལ should be placed above the root [ 'ཀ', 'ག', 'ང', 'ཅ', 'ཇ', 'ཏ', 'ད', 'པ', 'བ', 'ཧ' ]>>"

parse_2_la :: Parser Text
parse_2_la = do
    superfix <- pSuperfixLa
    root <- pLaSuperfixRoot
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root
    pure $ maybe consT (consT :>) vowel
    
-- 
-- Roots under superfix 'ས' are [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'པ', 'བ', 'མ', 'ཙ' ]
saSuperfixRoot :: HashSet Char
saSuperfixRoot = fetchChars subConsonants [1, 3, 4, 8, 9, 11, 12, 13, 15, 16, 17]

pSaSuperfixRoot :: Parser Char
pSaSuperfixRoot = satisfy (`member` laSuperfixRoot) <?> "<<Superfix ས should be placed above the root [ 'ཀ', 'ག', 'ང', 'ཉ', 'ཏ', 'ད', 'ན', 'པ', 'བ', 'མ', 'ཙ' ]>>"

parse_2_sa :: Parser Text
parse_2_sa = do
    superfix <- pSuperfixSa
    root <- pSaSuperfixRoot
    vowel <- optional pVowel
    eof
    let consT = T.empty :> superfix :> root
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither parse_2_sa "སྒ"
-- Right "སྒ"
