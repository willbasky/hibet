{-
Tibetan spelling grammar 4.16 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint16
    ( pConstraint16Da
    , pConstraint16Sa
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Word (Position (..), TibetanWord, mark)
import Convert.Token
    ( Consonant (..)
    , Token
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import qualified Text.Megaparsec as MP

pConstraint16Da :: Spelling -> Parser TibetanWord
pConstraint16Da _spelling = mark Suffix pAllowedSuffix16Da

pConstraint16Sa :: Spelling -> Parser TibetanWord
pConstraint16Sa _spelling = mark Suffix pAllowedSuffix16Sa

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
