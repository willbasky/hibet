{-
Tibetan spelling grammar 4.16 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint16
    ( pConstraint16Da
    , pConstraint16Sa
    , suffixGroup16Da
    , suffixGroup16Sa
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token
    ( Consonant (..)
    , Token
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import qualified Text.Megaparsec as MP

pConstraint16Da :: Spelling -> Parser TibetanSyllable
pConstraint16Da _spelling = mark Suffix pAllowedSuffix16Da

pConstraint16Sa :: Spelling -> Parser TibetanSyllable
pConstraint16Sa _spelling = mark Suffix pAllowedSuffix16Sa

-- | The 2nd-suffix rule (4.16): the first suffix the postfix ད may follow,
-- [ 'ན', 'ར', 'ལ' ]. The same set is the data of the generic word's
-- word-tail window ('Convert.Grammar.Constraint.Constraint21'), so the rule
-- lives here once.
suffixGroup16Da :: [Consonant]
suffixGroup16Da = [Cn, Cr, Cl]

-- | The 2nd-suffix rule (4.16): the first suffix the postfix ས may follow,
-- [ 'ག', 'ང', 'བ', 'མ' ]. The same set is the data of the generic word's
-- word-tail window, so the rule lives here once.
suffixGroup16Sa :: [Consonant]
suffixGroup16Sa = [Cg, Cng, Cb, Cm]

-- Suffix group [ 'ན', 'ར', 'ལ' ] before postfix ད.
pAllowedSuffix16Da :: Parser Token
pAllowedSuffix16Da = pAllowedConsonant suffixGroup16Da

-- Suffix group [ 'ག', 'ང', 'བ', 'མ' ] before postfix ས.
pAllowedSuffix16Sa :: Parser Token
pAllowedSuffix16Sa = pAllowedConsonant suffixGroup16Sa

pAllowedConsonant :: [Consonant] -> Parser Token
pAllowedConsonant allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
