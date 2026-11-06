{-
Tibetan spelling grammar 4.9 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint09
    ( pConstraint09
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
import Convert.Grammar.Superfix (rootSubfixRows)
import Convert.Grammar.Syllable (TibetanSyllable)
import Convert.Token
    ( Consonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

pConstraint09 :: SpellParser TibetanSyllable
pConstraint09 =
    MP.choice
        [ MP.try $ parseConstraint09 GP.pSubfixWa pRootSubfixWa
        , MP.try $ parseConstraint09 GP.pSubfixYa pRootSubfixYa
        , MP.try $ parseConstraint09 GP.pSubfixRa pRootSubfixRa
        , MP.try $ parseConstraint09 GP.pSubfixLa pRootSubfixLa
        ]

parseConstraint09 ::
    Parser Token -> Parser Token -> SpellParser TibetanSyllable
parseConstraint09 parseSubfix parseRoot = do
    root <- markRoot parseRoot
    subfix <- markSubfix parseSubfix
    vowel <- vowelSlot (root <> subfix) [markVowel GP.pVowel]
    pure (root <> subfix <> vowel)

pRootSubfixWa :: Parser Token
pRootSubfixWa = pAllowedRoot (row Cw)

pRootSubfixYa :: Parser Token
pRootSubfixYa = pAllowedRoot (row Cy)

pRootSubfixRa :: Parser Token
pRootSubfixRa = pAllowedRoot (row Cr)

pRootSubfixLa :: Parser Token
pRootSubfixLa = pAllowedRoot (row Cl)

row :: Consonant -> [Consonant]
row sub = fromMaybe [] (lookup sub rootSubfixRows)

pAllowedRoot :: [Consonant] -> Parser Token
pAllowedRoot allowed = do
    tok <- GP.pRootConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
