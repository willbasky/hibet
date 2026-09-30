{-
Tibetan spelling grammar 4.8 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint08
    ( pConstraint08
    , lSuperfixRoots
    , rSuperfixRoots
    , sSuperfixRoots
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant, TcSubConsonant)
    , toSubjoined
    , tokenCanonical
    )
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Text.Megaparsec as MP

-- | The roots the superfix ར may gate (rule 4.8). The same set is the data of
-- the generic word's superfix window
-- ('Convert.Grammar.Constraint.Constraint21'), so the rule lives here once.
rSuperfixRoots :: [Consonant]
rSuperfixRoots = [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cb, Cm, Cts, Cdz]

-- | The roots the superfix ལ may gate (rule 4.8); shared the same way.
lSuperfixRoots :: [Consonant]
lSuperfixRoots = [Ck, Cg, Cng, Cc, Cj, Ct, Cd, Cp, Cb, Ch]

-- | The roots the superfix ས may gate (rule 4.8); shared the same way.
sSuperfixRoots :: [Consonant]
sSuperfixRoots = [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cp, Cb, Cm, Cts]

pConstraint08 :: Spelling -> Parser TibetanSyllable
pConstraint08 = \case
    Tibetan ->
        MP.choice
            [ MP.try $ parseConstraintUnicode08 GP.pSuperfixRa pRaSuperfixRootUnicode
            , MP.try $ parseConstraintUnicode08 GP.pSuperfixLa pLaSuperfixRootUnicode
            , MP.try $ parseConstraintUnicode08 GP.pSuperfixSa pSaSuperfixRootUnicode
            ]
    Wylie ->
        MP.choice
            [ MP.try $ parseConstraintWylie08 GP.pSuperfixRa pRaSuperfixRootWylie
            , MP.try $ parseConstraintWylie08 GP.pSuperfixLa pLaSuperfixRootWylie
            , MP.try $ parseConstraintWylie08 GP.pSuperfixSa pSaSuperfixRootWylie
            ]

parseConstraintUnicode08 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintUnicode08 parseSuperfix parseRoot = do
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.optional (mark Vowel GP.pVowel)
    pure (superfix <> root <> fromMaybe mempty vowel)

-- | The root groups under the superfix, as subjoined signs: the full-letter
-- groups of the rule mapped to their joined forms.
pRaSuperfixRootUnicode :: Parser Token
pRaSuperfixRootUnicode = pAllowedSubConsonant (mapMaybe toSubjoined rSuperfixRoots)

pLaSuperfixRootUnicode :: Parser Token
pLaSuperfixRootUnicode = pAllowedSubConsonant (mapMaybe toSubjoined lSuperfixRoots)

pSaSuperfixRootUnicode :: Parser Token
pSaSuperfixRootUnicode = pAllowedSubConsonant (mapMaybe toSubjoined sSuperfixRoots)

pAllowedSubConsonant :: [SubConsonant] -> Parser Token
pAllowedSubConsonant allowed = do
    tok <- GP.pSubConsonant
    case tokenCanonical tok of
        TcSubConsonant sc
            | sc `elem` allowed -> pure tok
        _ -> MP.empty

-- The Wylie arm: the same rule, the root under the superfix is a full letter
-- and the vowel is always written, so it only shares the skeleton with the
-- unicode arm above.

-- | The Wylie skeleton: the vowel slot is obligatory (see 'parseConstraintWylie08').
parseConstraintWylie08 ::
    Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintWylie08 parseSuperfix parseRoot = do
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
    pure (superfix <> root <> vowel)

-- | The same root groups for Wylie spelling: the root under the superfix is a
-- full letter ("rka" -> རྐ), where Tibetan writes the joined sign.
pRaSuperfixRootWylie :: Parser Token
pRaSuperfixRootWylie = pAllowedConsonant rSuperfixRoots

pLaSuperfixRootWylie :: Parser Token
pLaSuperfixRootWylie = pAllowedConsonant lSuperfixRoots

pSaSuperfixRootWylie :: Parser Token
pSaSuperfixRootWylie = pAllowedConsonant sSuperfixRoots

-- | A full consonant letter from a group (Wylie spells stack letters in full).
pAllowedConsonant :: [Consonant] -> Parser Token
pAllowedConsonant allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
