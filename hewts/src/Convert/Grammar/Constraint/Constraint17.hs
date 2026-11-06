{-
Tibetan spelling grammar 4.17 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint17
    ( pConstraint17Ra
    , pConstraint17Ya
    ) where

import Convert.Grammar.Parser
    ( Parser
    , SpellParser
    , markRoot
    , markSubfix
    , markVowel
    , vowelSlot
    )
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token
    ( Consonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import qualified Text.Megaparsec as MP

-- (1) root group [ 'ག', 'ད' ] above the subfix ར.
pConstraint17Ra :: SpellParser TibetanSyllable
pConstraint17Ra = do
    root <- markRoot (pRootConsonant [Cg, Cd])
    ra <- markSubfix GP.pSubfixRa
    wa <- markSubfix GP.pSubfixWa
    let stack = root <> ra <> wa
    vowel <- vowelSlot stack [markVowel GP.pVowel]
    pure (stack <> vowel)

-- (2) root ཕ above the subfix ཡ.
pConstraint17Ya :: SpellParser TibetanSyllable
pConstraint17Ya = do
    root <- markRoot (pRootConsonant [Cph])
    ya <- markSubfix GP.pSubfixYa
    wa <- markSubfix GP.pSubfixWa
    let stack = root <> ya <> wa
    vowel <- vowelSlot stack [markVowel GP.pVowel]
    pure (stack <> vowel)

pRootConsonant :: [Consonant] -> Parser Token
pRootConsonant allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c | c `elem` allowed -> pure tok
        _ -> MP.empty
