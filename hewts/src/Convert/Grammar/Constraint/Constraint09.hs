{-
Tibetan spelling grammar 4.9 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint09
    ( pConstraint09
    , rootSubfixRows
    ) where

import Convert.Grammar.Parser (Parser, SpellParser, mark, vowelSlot)
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable)
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
    root <- mark Root parseRoot
    subfix <- mark Subfix parseSubfix
    vowel <- vowelSlot (root <> subfix) [mark Vowel GP.pVowel]
    pure (root <> subfix <> vowel)

-- | The root groups of rule 4.9, one row per subjoined letter: the same data
-- the parsers below read and the notation's solid-run table uses
-- ('Convert.Wylie.Tables'), so the two cannot drift apart.
rootSubfixRows :: [(Consonant, [Consonant])]
rootSubfixRows =
    [ (Cw, [Ck, Ckh, Cg, Cny, Cd, Ctsh, Czh, Cz, Cr, Cl, Csh, Ch])
    , (Cy, [Ck, Ckh, Cg, Cp, Cph, Cb, Cm])
    , (Cr, [Ck, Ckh, Cg, Ct, Cth, Cd, Cp, Cph, Cb, Cm, Cs, Ch])
    , (Cl, [Ck, Cg, Cb, Cz, Cr, Cs])
    ]

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
