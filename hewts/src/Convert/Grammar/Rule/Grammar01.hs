{-
Tibetan spelling grammar 4.1 (token parser variant)
-}

module Convert.Grammar.Rule.Grammar01 where

import qualified Convert.Grammar.Parser as GP
import Convert.Token (Token)
import Data.Maybe (maybeToList)
import Text.Megaparsec (choice, optional)

pGrammar1 :: GP.Parser [Token]
pGrammar1 = do
    root <- GP.pRootConsonant
    vowel <- optional GP.pVowel
    pure (root : maybeToList vowel)

pGrammar1WithLong :: GP.Parser [Token]
pGrammar1WithLong = do
    root <- GP.pRootConsonant
    vowel <- optional $ choice [GP.pVowel, GP.pVowelLongA]
    pure (root : maybeToList vowel)


pGrammar1Sanskrit :: GP.Parser [Token]
pGrammar1Sanskrit = do
    root <- GP.pSanskrit
    vowel <- optional GP.pVowel
    pure (root : maybeToList vowel)
