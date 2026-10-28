{-
Tibetan spelling grammar 4.15 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint15 (pConstraint15) where

import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , markSuffix
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token
    ( Token
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import qualified Text.Megaparsec as MP

pConstraint15 :: SpellParser TibetanSyllable
pConstraint15 = markSuffix pAllowedSuffix

-- Suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ], the one
-- table of 4.3 in 'Convert.Grammar.Parser'.
pAllowedSuffix :: Parser Token
pAllowedSuffix = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` GP.suffixConsonants -> pure tok
        _ -> MP.empty
