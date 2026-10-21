{-
Tibetan spelling grammar 4.9 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint09
    ( pConstraint09
    , rootSubfixRows
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..), mark)
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

pConstraint09 :: Spelling -> Parser TibetanSyllable
pConstraint09 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode09 GP.pSubfixWa pRootSubfixWa
            , MP.try $ parseConstraintUnicode09 GP.pSubfixYa pRootSubfixYa
            , MP.try $ parseConstraintUnicode09 GP.pSubfixRa pRootSubfixRa
            , MP.try $ parseConstraintUnicode09 GP.pSubfixLa pRootSubfixLa
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie09 GP.pSubfixWaWylie pRootSubfixWa
            , MP.try $ parseConstraintWylie09 GP.pSubfixYaWylie pRootSubfixYa
            , MP.try $ parseConstraintWylie09 GP.pSubfixRaWylie pRootSubfixRa
            , MP.try $ parseConstraintWylie09 GP.pSubfixLaWylie pRootSubfixLa
            ]

parseConstraintUnicode09 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintUnicode09 parseSubfix parseRoot = do
    root <- mark Root parseRoot
    subfix <- mark Subfix parseSubfix
    vowel <- MP.optional (mark Vowel GP.pVowel)
    pure (root <> subfix <> fromMaybe mempty vowel)

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
row sub = maybe [] id (lookup sub rootSubfixRows)

pAllowedRoot :: [Consonant] -> Parser Token
pAllowedRoot allowed = do
    tok <- GP.pRootConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty

-- The Wylie arm: the root above the subfix is the same full letter either way
-- - only the subfix itself is a plain letter here, so both arms share the
-- root groups - and the vowel is always written.

parseConstraintWylie09 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintWylie09 parseSubfix parseRoot = do
    root <- mark Root parseRoot
    subfix <- mark Subfix parseSubfix
    vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
    pure (root <> subfix <> vowel)
