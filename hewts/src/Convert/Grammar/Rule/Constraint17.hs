{-
Tibetan spelling grammar 4.17 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint17
    ( pConstraint17Ra
    , pConstraint17Ya
    ) where

import Convert.Grammar.Parser (Parser)
import qualified Convert.Grammar.Parser as GP
import Convert.Token
  ( Consonant (..)
  , Token
  , TokenCanonical (TcConsonant)
  , tokenCanonical
  )
import qualified Text.Megaparsec as MP
import Data.Maybe (maybeToList)

-- (1) root group [ 'ག', 'ད' ] above the subfix ར.
pConstraint17Ra :: Parser [Token]
pConstraint17Ra = do
  root <- pRootConsonant [Cg, Cd]
  ra <- GP.pSubfixRa
  wa <- GP.pSubfixWa
  vowel <- MP.optional GP.pVowel
  pure (root : ra : wa : maybeToList vowel)

-- (2) root ཕ above the subfix ཡ.
pConstraint17Ya :: Parser [Token]
pConstraint17Ya = do
  root <- pRootConsonant [Cph]
  ya <- GP.pSubfixYa
  wa <- GP.pSubfixWa
  vowel <- MP.optional GP.pVowel
  pure (root : ya : wa : maybeToList vowel)

pRootConsonant :: [Consonant] -> Parser Token
pRootConsonant allowed = do
  tok <- GP.pConsonant
  case tokenCanonical tok of
    TcConsonant c | c `elem` allowed -> pure tok
    _ -> MP.empty