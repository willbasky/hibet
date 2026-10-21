{-
Tibetan spelling grammar 4.15 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint15 (pConstraint15, suffixConsonants15) where

import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , markSuffix
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token
    ( Consonant (..)
    , Token
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import qualified Text.Megaparsec as MP

pConstraint15 :: SpellParser TibetanSyllable
pConstraint15 = markSuffix pAllowedSuffix

-- | The suffix group of rule 4.15, kept as data of this constraint: the same
-- set decides which letters a syllable's second position may take, and which
-- of them makes a two-letter syllable readable the other way round
-- ('Convert.Grammar.Constraint.Constraint22').
suffixConsonants15 :: [Consonant]
suffixConsonants15 = [Cg, Cng, Cd, Cn, Cb, Cm, C', Cr, Cl, Cs]

-- Suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ]
pAllowedSuffix :: Parser Token
pAllowedSuffix = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` suffixConsonants15 -> pure tok
        _ -> MP.empty
