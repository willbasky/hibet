{-
Tibetan spelling grammar 4.8 (token parser variant)
-}

module Convert.Grammar.Constraint.Constraint08
    ( pConstraint08
    ) where

import Convert.Grammar.Parser (Parser, Spelling (..))
import qualified Convert.Grammar.Parser as GP
import Convert.Grammar.Syllable (Position (..), TibetanSyllable, mark)
import Convert.Token
    ( Consonant (..)
    , SubConsonant (..)
    , Token (..)
    , TokenCanonical (TcConsonant, TcSubConsonant)
    , tokenCanonical
    )
import Data.Maybe (fromMaybe)
import qualified Text.Megaparsec as MP

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

pRaSuperfixRootUnicode :: Parser Token
pRaSuperfixRootUnicode =
    pAllowedSubConsonant
        [SCk, SCg, SCng, SCj, SCny, SCt, SCd, SCn, SCb, SCm, SCts, SCdz]

pLaSuperfixRootUnicode :: Parser Token
pLaSuperfixRootUnicode =
    pAllowedSubConsonant
        [SCk, SCg, SCng, SCc, SCj, SCt, SCd, SCp, SCb, SCh]

pSaSuperfixRootUnicode :: Parser Token
pSaSuperfixRootUnicode =
    pAllowedSubConsonant
        [SCk, SCg, SCng, SCny, SCt, SCd, SCn, SCp, SCb, SCm, SCts]

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
parseConstraintWylie08 :: Parser Token -> Parser Token -> Parser TibetanSyllable
parseConstraintWylie08 parseSuperfix parseRoot = do
    superfix <- mark Superfix parseSuperfix
    root <- mark Root parseRoot
    vowel <- MP.choice [mark Vowel GP.pVowel, mark ImplicitVowel GP.pImplicitA]
    pure (superfix <> root <> vowel)

-- | The same root groups for Wylie spelling: the root under the superfix is a
-- full letter ("rka" -> རྐ), where Tibetan writes the joined sign.
pRaSuperfixRootWylie :: Parser Token
pRaSuperfixRootWylie =
    pAllowedConsonant
        [Ck, Cg, Cng, Cj, Cny, Ct, Cd, Cn, Cb, Cm, Cts, Cdz]

pLaSuperfixRootWylie :: Parser Token
pLaSuperfixRootWylie =
    pAllowedConsonant
        [Ck, Cg, Cng, Cc, Cj, Ct, Cd, Cp, Cb, Ch]

pSaSuperfixRootWylie :: Parser Token
pSaSuperfixRootWylie =
    pAllowedConsonant
        [Ck, Cg, Cng, Cny, Ct, Cd, Cn, Cp, Cb, Cm, Cts]

-- | A full consonant letter from a group (Wylie spells stack letters in full).
pAllowedConsonant :: [Consonant] -> Parser Token
pAllowedConsonant allowed = do
    tok <- GP.pConsonant
    case tokenCanonical tok of
        TcConsonant c
            | c `elem` allowed -> pure tok
        _ -> MP.empty
