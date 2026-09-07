{-
Tibetan spelling grammar 4.1
-}

module Convert.Grammar.Rules.Grammar01 (pGrammar1, pGrammar1WithLong, pGrammar1Sanskrit) where

import Convert.Grammar.Common

import Data.Char (chr)
import Data.HashSet (HashSet, fromList, member, singleton)
import Data.Text (Text, pattern (:<), pattern (:>))
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- Tibetan spelling grammar 4.1

pGrammar1 :: Parser Text 
pGrammar1 = do 
    root <- pRootConsonant
    vowel <- optional pVowel 
    let consT = T.empty :> root 
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pGrammar1 "ས"
-- Right "ས"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pGrammar1 "སུ"
-- No instance for `Show (Either Text Text)'
--   arising from a use of `prettyPrint'
-- In the first argument of `($)', namely `prettyPrint'
-- In the expression: prettyPrint $ parseEither pGrammar1 "སུ"
-- In an equation for `it_a11rE':
--     it_a11rE = prettyPrint $ parseEither pGrammar1 "སུ"

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pGrammar1 "ཐོ"
-- Right "ཐོ"

pGrammar1WithLong :: Parser Text 
pGrammar1WithLong = do 
    root <- pRootConsonant
    vowel <- optional $ choice [pVowel, vowelLongA] 
    let consT = T.empty :> root 
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pGrammar1WithLong "དུ"
-- Right "དུ"

pGrammar1Sanskrit :: Parser Text 
pGrammar1Sanskrit = do 
    root <- pSanskrit
    vowel <- optional pVowel 
    let consT = T.empty :> root 
    pure $ maybe consT (consT :>) vowel

-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Text.Pretty.Simple
-- >>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
-- >>> prettyPrint $ parseEither pGrammar1Sanskrit "ཌ"
-- Right "ཌ"
