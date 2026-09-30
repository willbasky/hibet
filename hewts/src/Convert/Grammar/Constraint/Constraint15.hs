{-
Tibetan spelling grammar 4.15 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint15 (pConstraint15, suffixConsonants15) where

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

pConstraint15 :: Spelling -> Parser TibetanSyllable
pConstraint15 _spelling = mark Suffix pAllowedSuffix

-- | The suffix group of rule 4.15, kept as data of this constraint: the same
-- set decides which letters a syllable's second position may take, and which
-- of them makes a two-letter syllable readable the other way round
-- ('Convert.Grammar.Constraint.Ambiguous').
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
