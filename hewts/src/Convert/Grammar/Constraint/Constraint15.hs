{-
Tibetan spelling grammar 4.15 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint15 (pConstraint15) where

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

-- Suffix group [ 'ག', 'ང', 'ད', 'ན', 'བ', 'མ', 'འ', 'ར', 'ལ', 'ས' ]
pAllowedSuffix :: Parser Token
pAllowedSuffix = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` suffixConsonants15 -> pure tok
        _ -> MP.empty

suffixConsonants15 :: [Consonant]
suffixConsonants15 = [Cg, Cng, Cd, Cn, Cb, Cm, C', Cr, Cl, Cs]
