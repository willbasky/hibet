{-
Tibetan spelling grammar 4.16 (token parser variant)
-}

module Convert.Grammar.Rule.Constraint16
    ( pConstraint16Da
    , pConstraint16Sa
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

pConstraint16Da :: Parser [Token]
pConstraint16Da = do
  tok <- pAllowedSuffix16Da
  pure [tok]

pConstraint16Sa :: Parser [Token]
pConstraint16Sa = do
  tok <- pAllowedSuffix16Sa
  pure [tok]

-- Suffix group [ 'ན', 'ར', 'ལ' ] before postfix ད.
pAllowedSuffix16Da :: Parser Token
pAllowedSuffix16Da = pAllowedConsonant [Cn, Cr, Cl]

-- Suffix group [ 'ག', 'ང', 'བ', 'མ' ] before postfix ས.
pAllowedSuffix16Sa :: Parser Token
pAllowedSuffix16Sa = pAllowedConsonant [Cg, Cng, Cb, Cm]

pAllowedConsonant :: [Consonant] -> Parser Token
pAllowedConsonant allowed = do
  tok <- GP.pConsonant
  case tokenCanonical tok of
    TcConsonant c
      | c `elem` allowed -> pure tok
    _ -> MP.empty